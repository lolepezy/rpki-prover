# Security & RFC-compliance review — rpki-prover

Reviewed at commit `e96b8f17` (branch `sqlite-memory-optimisation`), 2026-09-04.

Scope:

1. **RFC compliance of validation** — both stages: the per-object, self-contained
   `prevalidateObject` stage ([ObjectValidation.hs:678](src/RPKI/Validation/ObjectValidation.hs#L678))
   and the relational stage in [TopDown.hs](src/RPKI/Validation/TopDown.hs).
2. **Robustness** — crashes, unbounded resource use, and other non-semantic issues
   that can be triggered by data from the network.

Each item has a location, the reason it matters, and a concrete suggested change.

**Status:** items 1-11, 13-16, 18, 20-22 are fixed on branch `sqlite-security-fixes`
(commits `4ec56969`, `b6e470e6`, `4da979e8`). Item 17 turned out to be fixed as a
side effect of 4. Items 12 and 19, and one part of 15, are deliberately left open
-- see [Open items](#open) at the end. The "Where" locations below refer to the
code as it was *before* the fixes.

Two facts that set severity throughout:

* **Incremental validation is the shipped default.** `defaultConfig` says
  `FullEveryIteration`, but [Main.hs:936-937](app/Main.hs#L936-L937) overrides it to
  `Incremental` unless `--no-incremental-validation` is given. So the manifest-shortcut
  path is the normal path, not an opt-in.
* **Top-down validation runs in a separate worker process**
  ([Workflow.hs:585](src/RPKI/Workflow.hs#L585)), so a crash there does not kill the
  daemon — it produces "Validator process failed" and no new version. A *deterministic*
  crash still means RPKI data never refreshes again, which for an RP is a real
  availability failure, just not a process-level one.

Severity legend: **Critical** = breaks the RPKI trust model or compromises the host ·
**High** = wrong validation outcome or reliable denial of service · **Medium** =
deviation from an RFC with practical consequences · **Low** = strictness/robustness
polish.

---

## Summary table

| # | Severity | Area | Issue | Status |
|---|----------|------|-------|--------|
| [1](#1) | Critical | rsync fetch | Path traversal from certificate SIA into `rsync --delete` destination | fixed |
| [2](#2) | High | Top-down (incremental) | Revocation not enforced for unchanged manifest children | fixed |
| [3](#3) | High | Top-down (incremental) | Manifest-number-rollback check has an inverted condition | fixed |
| [4](#4) | High | Parsing | `succ`/`pred` overflow in AS resource normalisation → uncaught `ErrorCall` | fixed |
| [5](#5) | High | Prevalidation | `error` call reachable via duplicate `signing-time` CMS attribute | fixed |
| [6](#6) | Medium | Prevalidation | `eContentType` / outer `contentType` OIDs are never validated (RFC 6488 §2.1) | fixed |
| [7](#7) | Medium | Parsing | ASN values silently truncated `mod 2^32` in ROA / ASPA / SPL / RFC 3779 | fixed |
| [8](#8) | Medium | BGPsec | AS range in a router certificate expands to a list → memory exhaustion | fixed |
| [9](#9) | Medium | Parsing | RFC 3779 ranges/prefixes not range-checked (inverted ranges, bogus lengths, duplicate AFIs) | fixed |
| [10](#10) | Medium | BGPsec | EC curve and signature algorithm not constrained (RFC 8608) | fixed |
| [11](#11) | Medium | Prevalidation | EE certificate SIA `id-ad-signedObject` never checked (RFC 6487 §4.8.8.2) | fixed |
| [12](#12) | Medium | Top-down | Manifest selection orders by `thisUpdate`, not `manifestNumber` | **open** |
| [13](#13) | Medium | Robustness | Parse exceptions escape the local `catch` in the fetch paths | fixed |
| [14](#14) | Low | Prevalidation | Missing RFC 6487 profile checks (version, EKU on CA certs, signature algorithm) | fixed |
| [15](#15) | Low | ROA | Missing RFC 9582 content checks (empty ROA, duplicate AFI, `maxLength = 0`, AS resources on EE) | fixed (partly) |
| [16](#16) | Low | Manifest parser | Version check is inverted / unreachable | fixed |
| [17](#17) | Low | Resources | `subtractRange` off-by-one produces a wrong overclaim report | fixed via 4 |
| [18](#18) | Low | Time | `Instant` is `Int64` nanoseconds — dates after 2262 wrap silently | fixed |
| [19](#19) | Low | Performance | `worthParallelism` selects the sequential branch (inverted, two places) | **open** |
| [20](#20) | Low | RTR | No connection limit; PDU framing assumes one PDU per `recv` | fixed |
| [21](#21) | Low | HTTP API | `fromJust` on endpoints before the first validation completes | fixed |
| [22](#22) | Low | ASPA | Provider list ordering/duplication not enforced | fixed |

---

<a name="1"></a>
## 1. Path traversal in the rsync destination path — Critical

**Where:** [Rsync.hs:370-384](src/RPKI/Rsync.hs#L370-L384), fed from
[Util.hs:128](src/RPKI/Util.hs#L128) and
[Repository.hs:277-284](src/RPKI/Repository.hs#L277-L284).

```haskell
rsyncDestination rsyncMode root (RsyncURL (RsyncHost (RsyncHostName host) port) path) = do
    let fullPath = ((U.convert host :: String) <> portPath) :| map (U.convert . unRsyncPathChunk) path
    let mkPath p = foldl (</>) root p
    ...
    RsyncDirectory -> do
        createDirectoryIfMissing True target
        pure $ addTrailingPathSeparator target
```

The `RsyncPathChunk`s come straight from a CA certificate's SIA `id-ad-caRepository`
extension, via `parseRsyncURL` → `modern-uri`'s `mkURI`. Checking `modern-uri-0.3.6`:

* `mkURI` is `runParser (parser <* eof)` and `parser` never calls `removeDotSegments`
  — dot-segment removal only happens in `relativeTo`. So `..` survives as a path piece.
* `pchar` includes `percentEncChar`, which *decodes* `%XX` into the raw character, and
  the `PathPiece` refinement is only `not . T.null`. So `%2F` decodes to `/` inside a
  single path piece and `%2e%2e` to `..`.

`validateDerivedCertUris` only checks the `rsync://` prefix; nothing sanitises the path.

Two working shapes:

| SIA value | resulting `destination` |
|---|---|
| `rsync://evil.example/a/../../../../../etc/cron.d/` | `<rsyncRoot>/evil.example/a/../../../../../etc/cron.d` |
| `rsync://evil.example/%2Fetc%2Fcron.d/` | `/etc/cron.d` — `System.FilePath.</>` lets an absolute second component replace the accumulator outright |

That destination is then handed to
[Rsync.hs:355-367](src/RPKI/Rsync.hs#L355-L367):

```haskell
proc "rsync" $ ... <> [ "--recursive", "--delete", "--copy-links" ] <> [ sourceUrl, destination ]
```

`--delete` removes everything in the target directory that is not present at the
attacker's source, and the source's files are written there. Depending on the account
rpki-prover runs as, `/etc/cron.d`, `~/.ssh`, a systemd unit directory or the prover's
own binary/cache are all reachable — arbitrary file write and delete, i.e. code
execution in the common cases.

`createDirectoryIfMissing True target` also creates directory trees anywhere on the
filesystem before rsync even starts.

Reachability: the certificate carrying the SIA must validate (signature, resources,
CRL) before its publication point is merged into `Fetcheables`, so the attacker needs a
validly-signed CA certificate under some configured TA — i.e. any delegated CA in the
RPKI. That is squarely inside the RPKI threat model: a CA must not be able to affect
anything outside its own resource subtree, let alone the RP's filesystem. `proc` does
not use a shell, so there is no *command* injection; the problem is purely the
destination path.

**Fix**

* In `parseRsyncURL`, reject any URL whose decoded path pieces contain `.`, `..`, a path
  separator, a NUL, or a leading `/`; reject a host that is not a plausible reg-name or
  IP literal. Do this in the parser so every consumer benefits.
* Independently, harden `rsyncDestination`: build the path, then canonicalise
  (`makeAbsolute` / `canonicalizePath` on the parent) and assert the result is still
  under `rsyncRoot` before creating anything or invoking rsync. Fail with a validation
  error otherwise.
* Consider percent-encoding each chunk (or hashing the URL) for the on-disk name
  instead of using the decoded text as a filesystem path.
* A regression test with the two URLs in the table above is cheap and worth having.

---

<a name="2"></a>
## 2. Revocation is not enforced for unchanged manifest children — High

**Where:** [TopDown.hs:806-810](src/RPKI/Validation/TopDown.hs#L806-L810) and
[TopDown.hs:934-942](src/RPKI/Validation/TopDown.hs#L934-L942), with payload collection
at [TopDown.hs:1394-1444](src/RPKI/Validation/TopDown.hs#L1394-L1444).

On the incremental path, children present on both the previous shortcut and the new
manifest ("overlapping") are never re-validated. When the CRL changes, the only check
performed is:

```haskell
when (crlKey /= mftShort ^. #crlShortcut . #key) $ do
    increment topDownCounters.originalCrl
    checkForRevokedChildren mftShort keyedMft overlappingChildren validCrl
```

and `checkForRevokedChildren` emits **warnings**:

```haskell
when (isRevoked (getSerial mft) validCrl) $ vWarn RevokedResourceCertificate
forM_ children $ \(T3 _ _ childKey) ->
    for_ (Map.lookup childKey mftShortcut.nonCrlEntries) $ \MftEntry {..} ->
        for_ (getMftChildSerial child) $ \childSerial ->
            when (isRevoked childSerial validCrl) $
                vFocusOn ObjectFocus childKey $ vWarn RevokedResourceCertificate
```

`collectPayloads` is then called with exactly those overlapping children, and
`getChildPayloads` collects their payloads unconditionally — `validateShortcut` checks
only the validity period and (conditionally) resources. So:

> A CA revokes a ROA's EE certificate, publishes a new CRL, and leaves the ROA listed on
> the new manifest. rpki-prover logs a warning and **still emits the ROA's VRPs**.

The same holds for a revoked child *CA* certificate (`CaChild` → `validateCa (CaShort …)`,
which has no revocation check at all), and for ASPA/SPL/BGPsec/GBR children.

Compare with the full path, where `allowRevoked`
([TopDown.hs:1294-1302](src/RPKI/Validation/TopDown.hs#L1294-L1302)) catches
`RevokedResourceCertificate`, emits a warning **and drops the payload** — that is the
correct behaviour, and the incremental path diverges from it.

Also note the warnings are emitted in the child's own scope, so the `Set.null issues`
test that guards shortcut creation
([TopDown.hs:864](src/RPKI/Validation/TopDown.hs#L864)) does not see them, and the
shortcut is refreshed as if nothing happened.

**Fix**

* Make `checkForRevokedChildren` fail the child rather than warn: drop the child's
  payload and replace its shortcut entry with `makeChildWithIssues` so the next round
  re-validates it from scratch.
* Run the revocation check for *all* overlapping children whenever the CRL object
  changed, not only in the `manifestFullValidation` diff branch — and make sure the
  `onlyCollectPayloads` path cannot be reached with a CRL that differs from the one the
  shortcut was built against. (Today it can't, because the manifest pins the CRL hash,
  but the invariant should be asserted rather than assumed.)
* Consider storing the revocation-relevant serial in the shortcut and re-checking it on
  every run; it is one `Set.member` per child and removes the whole class of staleness.

---

<a name="3"></a>
## 3. Manifest-number rollback: inverted condition — High

**Where:** [TopDown.hs:818-833](src/RPKI/Validation/TopDown.hs#L818-L833).

```haskell
let mftNumber = mft.content.mftNumber
when (mftNumber < mftShort.manifestNumber) $ do
    -- comment: vError triggers fall-back to the shortcut; but if the shortcut
    -- has expired there is nothing to fall back to, so only warn in that case
    let ValidityPeriod { notBefore = beforeMft, notAfter = afterMft } = getValidityPeriod mftShort
    let issue = ManifestNumberDecreased mftShort.manifestNumber mftNumber
    if beforeMft < unNow now && unNow now > afterMft
        then vError issue
        else vWarn issue
```

`beforeMft < now && now > afterMft` is true exactly when the shortcut **has expired** —
the opposite of what the comment describes. The consequences are both wrong:

* Shortcut still valid (the common case) → only `vWarn`, and validation continues with
  the **older** manifest. That is manifest replay/rollback being accepted, which is
  precisely what the check exists to prevent (RFC 9286 §6.6).
* Shortcut expired → `vError` with nothing to fall back to → the CA subtree fails.

**Fix**

```haskell
if isWithinValidityPeriod now mftShort
    then vError issue   -- there is a valid shortcut to fall back to
    else vWarn issue    -- nothing to fall back to, proceed but complain
```

`isWithinValidityPeriod` already exists at
[ObjectValidation.hs:667](src/RPKI/Validation/ObjectValidation.hs#L667). Add a test
covering both branches.

---

<a name="4"></a>
## 4. `succ`/`pred` overflow in AS-resource normalisation — High

**Where:** [Resources.hs:147-188](src/RPKI/Resources/Resources.hs#L147-L188)
(`normaliseAsns`), [Resources.hs:238-260](src/RPKI/Resources/Resources.hs#L238-L260)
(`subtractAsn`), [Resources.hs:96-102](src/RPKI/Resources/Resources.hs#L96-L102)
(`subtractRange`).

`ASN` is `newtype ASN = ASN Word32 ... deriving newtype Enum`
([Types.hs:43-46](src/RPKI/Resources/Types.hs#L43-L46)), so `succ (ASN maxBound)` throws
`Enum.succ{Word32}: tried to take 'succ' of maxBound` — verified on GHC 9.10.3.
`normaliseAsns` applies `succ` to the *end* of a range:

```haskell
tryMerge (ASRange a00 a01) (ASRange a10 a11)
    | succ a01 >= a10 = Just $ ASRange a00 (max a01 a11)
```

Reproduced with a verbatim copy of `normaliseAsns`:

```
[0-max, 10-20]  -> CRASH: Enum.succ{Word32}: tried to take `succ' of maxBound
[AS max, max-0] -> CRASH: Enum.succ{Word32}: tried to take `succ' of maxBound
[1-5, 7-9]      -> [ASRange 1 5,ASRange 7 9]
```

So an RFC 3779 AS block of `{0-4294967295, 10-20}` — "all ASNs" plus any second entry,
which is trivially constructible — crashes normalisation. `normaliseAsns` runs inside
`toRS` → `IS.fromList` → `V.fromList`, i.e. **during parsing**, before any signature
check. Anyone who can publish an object at any RPKI publication point can trigger it.

Related, same root cause:

* `subtractAsn (ASRange a0 a1) (AS b) | a0 == b = [ASRange (succ a0) a1]` — crashes on
  `a0 == maxBound`, reachable because `normaliseAsns`'s `simplify` collapses only
  `ASRange a a`, so *inverted* ranges (`ASRange maxBound 0`) survive into the
  `IntervalSet`.
* `subtractRange` uses `pred f2` and `succ l2` on `V4.IpAddress` / `V6.IpAddress`, which
  are also GND-derived `Enum` over `Word32` / `Word128`. `pred (IpAddress 0)` throws.
  Reachable when a child claims a prefix starting at `0.0.0.0` while the parent holds a
  prefix that also starts at `0.0.0.0` but is smaller — or symmetrically for `succ l2`
  when the parent's prefix ends at `255.255.255.255`. Narrower than the ASN case (needs
  a parent holding such a prefix and a real overclaim), but the same class.

Blast radius depends on where the thunk is forced (see [#13](#13)): object-level in the
RRDP snapshot path, delta-wide or repository-wide in the delta/rsync paths, and TA-wide
when re-parsed by `resolveTroubledChildByKey`
([TopDown.hs:1536-1548](src/RPKI/Validation/TopDown.hs#L1536-L1548)).

**Fix**

* Never use `succ`/`pred` on bounded types here. Lift to `Integer` (or use saturating
  helpers) for the adjacency tests: `a01 /= maxBound && succ a01 >= a10`, or compare
  `toInteger a01 + 1 >= toInteger a10`. Same for `subtractAsn` and `subtractRange`.
* Reject inverted ranges at parse time (see [#9](#9)) so they never reach the interval
  machinery.
* Add QuickCheck properties over `normaliseAsns` / `subtractAsn` / `subtract` with
  generators that include `minBound`, `maxBound`, and inverted ranges. The current
  generators almost certainly never produce them.

---

<a name="5"></a>
## 5. `error` reachable from a crafted CMS object — High

**Where:** [ObjectValidation.hs:753-768](src/RPKI/Validation/ObjectValidation.hs#L753-L768).

```haskell
-- It is guaranteed by validateCmsStructure; keep total pattern here
-- to satisfy -Wincomplete-uni-patterns.
signingTime =
    case [ newInstant dt | SigningTime dt _ <- attrs ] of
        [st] -> st
        _    -> error "Invariant violated: expected exactly one SigningTime attribute"
```

The invariant is *not* guaranteed. `validateCmsStructure`
([ObjectValidation.hs:852-853](src/RPKI/Validation/ObjectValidation.hs#L852-L853)) only
checks presence:

```haskell
when (Prelude.null [ () | SigningTime _ _ <- attrs ]) $ vError SigningTimeMissing
```

and the attribute parser (`parseSA`,
[SignedObject.hs:128-147](src/RPKI/Parse/Internal/SignedObject.hs#L128-L147)) happily
returns a list with two `SigningTime` entries. `messageDigest` has the correct
`case … of [x] -> …; _ -> vError` shape; `signingTime` and `contentType` do not.

`WellStructuredCms` is declared under `StrictData` ([Domain.hs:3](src/RPKI/Domain.hs#L3),
[Domain.hs:1007](src/RPKI/Domain.hs#L1007)), so building the record forces `signingTime`
and the `ErrorCall` fires immediately — inside `runValidatorT`, i.e. outside the local
`catch` (see [#13](#13)).

This is also the RFC 9589 requirement that RFC 6488 signed objects carry **exactly**
three signed attributes, so the fix closes a compliance gap too.

**Fix**

In `validateCmsStructure`, replace the three presence checks with exact-cardinality
checks and reject duplicates:

```haskell
case [ () | ContentTypeAttr _ <- attrs ] of { [_] -> pure (); _ -> vError ContentTypeAttrMissing }
case [ () | SigningTime _ _  <- attrs ] of { [_] -> pure (); _ -> vError SigningTimeMissing }
-- messageDigest already does this
when (length attrs /= 3) $ vError <someNewError>
```

and then make `extractCMSObject` total — take the signing time from a value threaded out
of `validateCmsStructure` rather than re-scanning, or fall back to a validation error
instead of `error`. Grep for other `error`/partial patterns justified by "guaranteed
elsewhere"; the guarantee should be expressed in a type or re-checked.

---

<a name="6"></a>
## 6. `eContentType` and outer `contentType` OIDs are never checked — Medium

**Where:** [ObjectValidation.hs:880-886](src/RPKI/Validation/ObjectValidation.hs#L880-L886);
constants at [Common.hs:70-77](src/RPKI/Parse/Internal/Common.hs#L70-L77).

The only content-type check is *internal consistency*:

```haskell
ContentTypeAttr ct ->
    unless (ct == eContentType scEncapContentInfo) $ vError EECertContentTypeMismatch
```

Nothing checks that:

* `ContentInfo.contentType` is `id-signedData` (1.2.840.113549.1.7.2) — RFC 6488 §2.1;
* `encapContentInfo.eContentType` is the OID for the object's actual type —
  RFC 6488 §2.1.3.1, RFC 9286 §4.2.1, RFC 9582 §4, RFC 6493 §5, RFC 9323 §3, and the
  ASPA / SPL profiles.

`id_ct_signedChecklist`, `id_ct_aspa` and `id_ct_rpkiSignedPrefixList` are defined in
`Common.hs` and never referenced anywhere — which is itself the tell.

Practical impact is bounded (the payload parser is chosen by the file extension, so a
mismatched OID mostly means the object is *accepted* where other RPs reject it), but
that asymmetry is exactly the kind of thing that produces divergent VRP sets between
implementations.

**Fix**

Thread the expected OID into `prevalidateObject` (it already switches on the object
type) and check both OIDs there:

| type | eContentType |
|---|---|
| MFT | `1.2.840.113549.1.9.16.1.26` |
| ROA | `1.2.840.113549.1.9.16.1.24` |
| GBR | `1.2.840.113549.1.9.16.1.35` |
| RSC | `id_ct_signedChecklist` |
| ASPA | `id_ct_aspa` |
| SPL | `id_ct_rpkiSignedPrefixList` |

(Add the manifest / ROA / GBR OIDs to `Common.hs` alongside the three that already
exist.)

---

<a name="7"></a>
## 7. AS numbers silently truncated mod 2^32 — Medium

**Where:**

* [Common.hs:431](src/RPKI/Parse/Internal/Common.hs#L431) — `as' = ASN . fromInteger`
  (RFC 3779 AS resources)
* [ROA.hs:53](src/RPKI/Parse/Internal/ROA.hs#L53) — `VrpsPerAs (ASN $ fromIntegral asId) …`
* [Aspa.hs:50,53](src/RPKI/Parse/Internal/Aspa.hs#L50-L53) — customer and providers
* [SPL.hs:46](src/RPKI/Parse/Internal/SPL.hs#L46) — `SplPayload (ASN asId) …`

`getInteger` yields an unbounded `Integer`; `fromInteger` / `fromIntegral` into `Word32`
wraps. A ROA with `asID = 4294967303` produces a VRP for **AS 7**. Every profile
constrains these to `INTEGER (0..4294967295)` (RFC 9582 §4.1, RFC 3779 §3.2.3, ASPA
profile §3), so conforming RPs reject the object while rpki-prover emits a VRP for an
unrelated ASN. That is a VRP-set divergence an attacker chooses.

**Fix**

Add a checked constructor and use it at all four sites:

```haskell
mkAsn :: Integer -> Either String ASN
mkAsn i | i < 0 || i > 4294967295 = Left $ "ASN out of range: " <> show i
        | otherwise               = Right $ ASN (fromInteger i)
```

Same treatment for `CMSVersion . fromInteger` at
[SignedObject.hs:58](src/RPKI/Parse/Internal/SignedObject.hs#L58) — a huge integer can
currently wrap onto the accepted value 3.

---

<a name="8"></a>
## 8. BGPsec AS range expands into a list — Medium

**Where:** [Resources.hs:274-281](src/RPKI/Resources/Resources.hs#L274-L281), used by
`validateBgpCertAsns`
([ObjectValidation.hs:625-632](src/RPKI/Validation/ObjectValidation.hs#L625-L632)).

```haskell
unwrapAsns = mconcat . map (\case
    AS asn -> [asn]
    ASRange a1 a2 | a1 >= a2  -> []
                  | otherwise -> [ a1 .. a2 ])
```

A BGPsec router certificate declaring `0-4294967295` yields a 2^32-element `[ASN]`,
stored in `BGPSecPayload.bgpSecAsns` ([Domain.hs:697-704](src/RPKI/Domain.hs#L697-L704)),
which is then forced by `Set.fromList` in `validateTA`
([TopDown.hs:333](src/RPKI/Validation/TopDown.hs#L333)) and serialised to the DB. That
is tens of GB — the validation worker dies on its RTS memory cap
(`validationWorkerMemoryMb`, default 2048), every cycle.

`validateBgpCertStructure` calls `void $ validateBgpCertAsns`, which does not force the
list, so prevalidation does not catch it; only the top-down stage does, by dying.

The `a1 >= a2 -> []` branch is also subtly wrong: `ASRange a a` should yield `[a]`. In
practice `normaliseAsns` collapses that case first, so it is latent.

**Fix**

* Cap the number of ASNs a BGPsec certificate may declare (RFC 8209 does not bound it,
  but a real router certificate has a handful; a few thousand is generous) and emit a
  validation error above the cap.
* Better: keep `bgpSecAsns` as the interval set and only expand when serialising RTR
  Router Key PDUs, where the same bound applies.
* Fix the `a1 >= a2` guard to `a1 > a2 -> []`.

---

<a name="9"></a>
## 9. RFC 3779 encodings are not range-checked — Medium

**Where:** [Common.hs:313-367](src/RPKI/Parse/Internal/Common.hs#L313-L367)
(`parseIpExt'`, `ipvVxAddress`), [Common.hs:422-431](src/RPKI/Parse/Internal/Common.hs#L422-L431)
(`asOrRange`).

Four separate gaps:

1. **Inverted ranges accepted.** `ipvVxAddress`'s two-`BitString` branch calls
   `rangeToPrefixes w1 w2` with no `w1 <= w2` check, and `asOrRange` builds `ASRange b e`
   with no `b <= e` check. RFC 3779 §2.2.3.9 / §3.2.3.8 require `min <= max`. An inverted
   IP range does not loop (hw-ip's `splitIpRange` terminates), but it silently yields a
   *different, arbitrary* prefix set: `Range 10.0.0.0 1.0.0.0` becomes `10.0.0.0/31`.
   Inverted AS ranges are the input that reaches the `succ maxBound` crash in [#4](#4).
2. **Prefix length not bounded.** `makePrefix bs nzBits` casts the BIT STRING's
   non-zero-bit count straight to `Word8`. A certificate declaring 40 significant bits
   for an IPv4 prefix produces `IpNetMask 40`; hw-ip's `bitPower m = fromIntegral (32 - m)`
   computes `32 - 40` in `Word8` (= 248), so `blockSize` and therefore `lastIpAddress`
   are nonsense (`b - 1`), giving intervals whose "last" precedes their "first". No
   crash, but `contains` / `intersection` / `isInside` all misbehave on them. Enforce
   `nzBits <= 32` (IPv4) / `<= 128` (IPv6), and that the BIT STRING is no longer than
   4/16 octets.
3. **Unused bits not required to be zero.** RFC 3779 §2.1.1 requires the host bits of an
   encoded prefix to be zero. `fourW8sToW32` / `someW8ToW128` just pad; nothing rejects a
   non-canonical encoding, so the same prefix has several accepted encodings.
4. **Duplicate/unordered address families silently collapsed.**
   [Common.hs:320-321](src/RPKI/Parse/Internal/Common.hs#L320-L321):
   ```haskell
   rs []       = R.emptyRS
   rs (af : _) = af      -- second and later IPv4 blocks are dropped
   ```
   RFC 3779 §2.2.3.3 requires `IPAddrBlocks` to be ordered by ascending `addressFamily`
   with no duplicates. Taking the first and dropping the rest means another RP may
   compute a larger resource set from the same certificate. Similarly,
   `extractAddressaFamily` does `BS.take 2` and ignores a SAFI byte, which RFC 6487
   §4.8.10 forbids; and `parseAsnExt'` reads only the `[0]` `asnum` element, silently
   ignoring an `rdi` element that RFC 6487 §4.8.11 says MUST be absent.

**Fix:** validate all four at parse time and turn violations into parse/validation errors
rather than silent coercions.

---

<a name="10"></a>
## 10. BGPsec key and signature algorithm not constrained — Medium

**Where:** [ObjectValidation.hs:795](src/RPKI/Validation/ObjectValidation.hs#L795) and
[ObjectValidation.hs:969](src/RPKI/Validation/ObjectValidation.hs#L969).

```haskell
case pubKey of
    PubKeyEC _ -> pure ()
    _          -> vError $ InvalidPublicKey "BGPsec certificate must use an EC public key"
```

RFC 8608 §3.1 requires the router certificate's key to be ECDSA on **P-256**
(`secp256r1`) and the signature algorithm to be `ecdsa-with-SHA256`. Any curve is
currently accepted, and the signature algorithm OID is never inspected — the RSA branch
in `validateCertX509Structure` checks modulus and exponent but there is no equivalent
check that the *algorithm identifier* is `sha256WithRSAEncryption` (RFC 7935 §3) for
ordinary certificates either.

**Fix**

* For `PubKeyEC`, require a named curve equal to `SEC_p256r1`; reject `PubKeyEC_Prime`
  (explicit parameters) outright.
* Check `cwsSignatureAlgorithm` is `SignatureALG HashSHA256 PubKeyALG_RSA` for
  RSA-keyed certificates and `SignatureALG HashSHA256 PubKeyALG_EC` for BGPsec ones, and
  apply the same to `SignCRL.signatureAlgorithm` and the CMS `signatureAlgorithm`
  (RFC 9589).
* While there: `validateSKIMatchesPublicKey`
  ([ObjectValidation.hs:975-989](src/RPKI/Validation/ObjectValidation.hs#L975-L989))
  takes the *first* `BitString` out of `toASN1 pubKey []`. That is correct for
  named-curve EC and RSA, but it is an assumption worth asserting rather than
  pattern-matching loosely.

---

<a name="11"></a>
## 11. EE certificate SIA `id-ad-signedObject` never checked — Medium

**Where:** [ObjectValidation.hs:156-161](src/RPKI/Validation/ObjectValidation.hs#L156-L161)
(`validateEeCertExtensions`); no `id_ad_signedObject` OID exists in
[Common.hs](src/RPKI/Parse/Internal/Common.hs).

RFC 6487 §4.8.8.2 requires an EE certificate's SIA to contain exactly one
`id-ad-signedObject` accessDescription pointing at the signed object's own publication
location. rpki-prover checks the *CA* SIA (`caRepository`, `rpkiManifest`) and validates
the manifest and CRL locations, but performs no check at all on the EE SIA.

Consequence: an object can be published at a location unrelated to what its EE
certificate claims, and a `.roa`/`.asa`/`.spl` moved to a different name or location is
accepted. `validateMftChild` already warns on a filename/location mismatch
([TopDown.hs:1105-1109](src/RPKI/Validation/TopDown.hs#L1105-L1109)); the SIA check is
the authoritative version of the same idea.

**Fix:** add `id_ad_signedObject = oid_pkix <> [48, 11]`, require exactly one such
accessDescription with an `rsync://` URI in `validateCmsStructure`, and compare it
against the object's locations in the top-down stage (warning or error, matching how
`checkCrlLocation` / `validateMftLocation` are treated today).

---

<a name="12"></a>
## 12. Manifest selection prefers latest `thisUpdate` over highest `manifestNumber` — Medium

**Where:** [Store/Types.hs:47-50](src/RPKI/Store/Types.hs#L47-L50) and
[Database.hs:421-426](src/RPKI/Store/Database.hs#L421-L426).

```haskell
instance Ord MftMeta where
    compare a b = compare (a ^. #thisTime) (b ^. #thisTime) <>
                  compare (a ^. #nextTime) (b ^. #nextTime) <>
                  compare (a ^. #mftNumber) (b ^. #mftNumber)
```

`getMftsForAKI` returns `sortOn Down`, so candidates are tried newest-`thisUpdate` first
and `manifestNumber` is only a third-level tiebreak. RFC 9286 §6.2/§6.6 makes
`manifestNumber` the primary discriminator between multiple manifests for one CA. A CA
can therefore steer rpki-prover to a different manifest than other RPs by publishing two
manifests where the later `thisUpdate` carries the lower number.

**Fix:** order by `mftNumber` first, then `thisTime`, then `nextTime`. Note the same
`Ord` instance is used elsewhere — check callers before changing it, or add a dedicated
comparator for selection.

---

<a name="13"></a>
## 13. Parse exceptions escape the local `catch` in the fetch paths — Medium

**Where:** [RrdpFetch.hs:494-511](src/RPKI/RRDP/RrdpFetch.hs#L494-L511),
[RrdpFetch.hs:641-666](src/RPKI/RRDP/RrdpFetch.hs#L641-L666),
[Rsync.hs:289-311](src/RPKI/Rsync.hs#L289-L311).

All three sites have the shape:

```haskell
z <- liftIO $ runValidatorT scopes $ inSubLocationScope uri $ vHoist $ do
        ro <- readObjectOfType type_ blob
        prevalidateObject ro
(evaluate $! case z of …) `catch` (\(e :: SomeException) -> …)
```

The `catch` guards only the `case` on the already-computed `z`. But the parse result is
forced *inside* `runValidatorT`: `ExceptT`'s bind pattern-matches the `Either`, and
`prevalidateObject`'s `case rpkiObject of` forces the parsed object to WHNF — which,
under `StrictData`, forces the whole resource set. So the `ErrorCall`s from [#4](#4) and
[#5](#5) are thrown by the *first* statement, not the second, and the handler never runs.

What actually catches them, and with what blast radius:

| path | handler | radius |
|---|---|---|
| RRDP snapshot | `waitCatch` at [RrdpFetch.hs:523](src/RPKI/RRDP/RrdpFetch.hs#L523) | one object — fine |
| RRDP delta | `fromTry … $ wait a` in `addObject` | whole delta fails → forced snapshot re-download every cycle |
| rsync | `fromTry (UnspecifiedE …) $ wait a` in `saveStorable` | **whole repository fetch fails** |
| `resolveTroubledChildByKey` | only `fromTryM` at [TopDown.hs:519](src/RPKI/Validation/TopDown.hs#L519) | **whole TA fails** |
| `BottomUp.validateObjectItself` | caller-dependent | RSC verification / API request |

**Fix**

* Move the `catch` (or a `try`) around the `runValidatorT` call itself, and force the
  result deeply (`evaluate . force`, or `evaluate $! z`) inside the guarded region.
* Wrap `resolveTroubledChildByKey`'s re-parse in its own handler so a poisoned cached
  blob degrades that one child instead of the TA.
* Fixing [#4](#4) and [#5](#5) removes today's triggers, but the containment gap is worth
  closing on its own — it is the difference between "one bad object" and "no validation
  results".

---

<a name="14"></a>
## 14. Missing RFC 6487 certificate-profile checks — Low

`validateCertX509Structure` ([ObjectValidation.hs:945-971](src/RPKI/Validation/ObjectValidation.hs#L945-L971))
covers validity ordering, serial bounds and the RSA key. Not covered:

* **`certVersion` must be 3** (RFC 6487 §4.1). Never inspected.
* **Extended Key Usage must be absent on CA certificates** (RFC 6487 §4.8.5).
  `validateNoUnknownCriticalExtensions` rejects a *critical* EKU because
  `id_ce_extKeyUsage` is not in `allowedCriticalOIDs`, but a non-critical EKU on a CA
  certificate passes.
* **Subject/Issuer naming** (RFC 6487 §4.4-4.5: a single CN, optionally a serialNumber)
  is not checked at all. Low practical impact; record it as a known deviation if you
  don't want to enforce it.
* **CRL issuer** is matched only via AKI/SKI and signature
  ([ObjectValidation.hs:363-374](src/RPKI/Validation/ObjectValidation.hs#L363-L374)); the
  CRL's `issuer` DN is never compared to the CA's `subject`.

---

<a name="15"></a>
## 15. ROA content checks missing (RFC 9582 §4) — Low

**Where:** [ROA.hs:36-97](src/RPKI/Parse/Internal/ROA.hs#L36-L97),
[ObjectValidation.hs:405-450](src/RPKI/Validation/ObjectValidation.hs#L405-L450).

* `ipAddrBlocks` is `SEQUENCE SIZE (1..MAX)` and each `addresses` is
  `SEQUENCE SIZE (1..MAX)` — `getMany` accepts empty in both places, so an empty ROA
  parses and validates.
* Only one `ROAIPAddressFamily` per AFI is allowed; duplicates are not rejected
  (`mconcat` just concatenates them).
* `prefixMaxLength <= 0` is rejected, but `maxLength = 0` with prefix `0.0.0.0/0` is
  syntactically legal; this rejects a (useless but valid) ROA.
* The ROA EE certificate must carry IP resources and must **not** carry AS resources.
  `validateResourceExtensionsPresenceAndCriticality` only requires *one of* the two, and
  `validateRoa` never checks the AS side. Compare `validateSpl`, which does call
  `resourceSetMustBeEmpty` for the IP side — the ROA equivalent is missing.

---

<a name="16"></a>
## 16. Manifest version check is inverted and effectively dead — Low

**Where:** [MFT.hs:44-48](src/RPKI/Parse/Internal/MFT.hs#L44-L48).

```haskell
(IntVal version, IntVal manifestNumber, ASN1Time TimeGeneralized thisUpdateTime' _) -> do
    when (version /= 1) $ throwParseError $ "Unexpected manifest version: " ++ show version
```

RFC 9286 declares `version [0] INTEGER DEFAULT 0`, so (a) the accepted value should be 0,
not 1, and (b) under DER a DEFAULT value must be omitted entirely, so an explicit version
should be rejected. Separately, the branch matches a bare `IntVal`, whereas a tagged `[0]`
version decodes as `Start (Container Context 0)` — so this branch is unreachable for
correctly-encoded input anyway.

**Fix:** drop the branch, or make it parse `[0] EXPLICIT` and reject any explicitly
encoded version as a DER violation.

---

<a name="17"></a>
## 17. `subtractRange` off-by-one in the overclaim report — Low

**Where:** [Resources.hs:96-102](src/RPKI/Resources/Resources.hs#L96-L102).

```haskell
| f1 > f2  && l1 >= l2 -> fromRange (Range l2 l1)
```

Subtracting `[f2,l2]` from `[f1,l1]` when `f1 > f2` and `l1 >= l2` should yield
`[succ l2, l1]`, not `[l2, l1]` — `l2` belongs to the parent set and is not overclaimed.
The overclaimed set reported in `OverclaimedResources` therefore contains one extra
address/ASN. The nested (accepted) set is computed separately from `intersection`, so VRP
output is unaffected; only diagnostics are wrong. Fix together with [#4](#4), since the
correct form needs the overflow-safe `succ`.

---

<a name="18"></a>
## 18. `Instant` overflows for dates after 2262 — Low

**Where:** [Time.hs:18](src/RPKI/Time.hs#L18), [Time.hs:106-109](src/RPKI/Time.hs#L106-L109).

`Instant` is `Int64` nanoseconds since the epoch, so the representable range ends at
`2262-04-11`. `toNanos` multiplies without any range check, so a certificate with
`notAfter` in, say, year 9999 (used by some CAs as "no expiry") wraps to an arbitrary —
usually negative — value. Today that fails closed (`ObjectIsExpired`), but the behaviour
is silent and non-obvious, and specific out-of-range years wrap back into plausible
future values.

**Fix:** range-check in `newInstant` / `toNanos` and reject out-of-range times with an
explicit validation error rather than wrapping.

---

<a name="19"></a>
## 19. `worthParallelism` picks the sequential branch — Low (performance)

**Where:** [TopDown.hs:1014-1021](src/RPKI/Validation/TopDown.hs#L1014-L1021) and
[TopDown.hs:1351-1364](src/RPKI/Validation/TopDown.hs#L1351-L1364).

```haskell
worthParallelism = length nonCrlChildren > 500
forAllChildren = if worthParallelism
                    then forM                      -- sequential
                    else pooledForConcurrentlyN 2  -- concurrent
```

Both occurrences read as inverted: when parallelism is judged worthwhile the code takes
the sequential branch. Large manifests are exactly the case where serialisation costs
wall-clock time and pushes against `topDownTimeout`. `length` also forces the whole list
before any work starts.

If this is deliberate (e.g. to bound memory on huge manifests) the variable deserves a
different name and a comment. Otherwise swap the branches and consider `null . drop 500`
instead of `length`.

---

<a name="20"></a>
## 20. RTR server: no connection limit, one-PDU-per-`recv` framing — Low

**Where:** [RtrServer.hs:102-109](src/RPKI/RTR/RtrServer.hs#L102-L109),
[RtrServer.hs:191](src/RPKI/RTR/RtrServer.hs#L191),
[RtrServer.hs:254](src/RPKI/RTR/RtrServer.hs#L254).

* `loop sock = forever $ accept …; forkFinally …` accepts unboundedly many connections.
  Each spawns two threads, a `dupTChan` and a bounded outbox. Per-connection memory is
  small (the broadcast channel only carries `NotifyPdu`, rate-limited to one per minute
  per RFC 8210 §8.2 — nicely done), but thread/FD exhaustion is unbounded.
* `recv connection 1024` treats one TCP read as exactly one PDU. A PDU split across
  segments yields a parse error, and pipelined PDUs after the first in a segment are
  dropped. RTR PDUs are small so this usually works, but it is not correct framing.

**Fix:** cap concurrent connections (a `QSem`/`TVar` counter, with a configurable limit)
and add an idle timeout; buffer per connection and parse using the PDU length field from
the header rather than relying on read boundaries.

---

<a name="21"></a>
## 21. `fromJust` on API endpoints before the first validation — Low

**Where:** [HttpServer.hs:304](src/RPKI/Http/HttpServer.hs#L304),
[HttpServer.hs:316](src/RPKI/Http/HttpServer.hs#L316),
[HttpServer.hs:329](src/RPKI/Http/HttpServer.hs#L329).

`getValuesByVersion`'s `getLatest` returns `Nothing` when `DB.getLatestVersion` finds no
version ([HttpServer.hs:226-229](src/RPKI/Http/HttpServer.hs#L226-L229)) — i.e. on a
fresh installation before the first validation run completes. The three call sites pass
`fromJust` as the converter, so `/api/validations`, `/api/validations-original` and
`/api/metrics` throw `Prelude.fromJust: Nothing` and return an opaque 500.

**Fix:** replace `fromJust` with an explicit `maybe (throwError err404 {…}) pure`.

---

<a name="22"></a>
## 22. ASPA provider list ordering/duplication not enforced — Low

**Where:** [Aspa.hs:49-54](src/RPKI/Parse/Internal/Aspa.hs#L49-L54).

`providers <- fmap Set.fromList $ … getMany …` silently sorts and deduplicates. The ASPA
profile requires `providers` to be sorted in ascending order with no duplicates; a
non-conforming object should be rejected, not normalised. Parse into a list, verify
strict ascending order, then convert.

Also worth revisiting: `validateAspaCore`
([ObjectValidation.hs:600-622](src/RPKI/Validation/ObjectValidation.hs#L600-L622)) rejects
`AS 0` mixed with non-zero providers (`AspaAsZeoAndNonZero`), which comes from an older
draft. Confirm against the current ASPA profile text before the next release.

---

## Things that look right

Worth recording so they don't get "fixed" later:

* CMS envelope parsing is genuinely strict: `onNextContainer` rejects leftover state, so
  exactly one `SignerInfo`, exactly one embedded certificate, and an absent `crls` field
  are all enforced structurally
  ([SignedObject.hs:50-56](src/RPKI/Parse/Internal/SignedObject.hs#L50-L56)).
* `messageDigest` is verified against `SHA256(eContent)` with the declared digest OID, and
  only SHA-256 is accepted, in both `digestAlgorithms` and the manifest `fileHashAlg`.
* SKI is verified to be SHA-1 of the subjectPublicKey bit string, and the CMS SID is
  matched against it.
* CRL profile checks are thorough: v2 only, `AKI` + `crlNumber` extensions only, no
  duplicates, no CRL entry extensions.
* Manifest filename charset and the "exactly one dot" rule match RFC 9286 §4.2.2.
* `validateAIA`, `validateMftLocation` and `checkCrlLocation` implement the AIA / SIA /
  CRLDP location cross-checks.
* RRDP notification/snapshot/delta hostname pinning
  ([RrdpFetch.hs:196-212](src/RPKI/RRDP/RrdpFetch.hs#L196-L212)) is a good anti-abuse
  measure the RFC does not require.
* Download size is capped *after* decompression (`sinkGenSize` sits on the decompressed
  conduit), so a compression bomb is bounded by `rrdpConf.maxSize`.
* No SQL injection: dynamic query text only ever splices in statically-known column names
  and generated `:kN` placeholders; all values are bound
  ([Database.hs:221-229](src/RPKI/Store/Database.hs#L221-L229)).
* `V.unsafeIndex` in `findFullIntersections`
  ([IntervalContainers.hs:45-103](src/RPKI/Resources/IntervalContainers.hs#L45-L103)) is
  bounds-safe on every path traced.
* `convert` (ByteString→Text) uses `decodeUtf8With lenientDecode`, so malformed UTF-8 in
  URLs cannot throw. The one partial decoder, `RPKI.Util.textual`, is only applied to
  rsync's own stdout/stderr and to Prometheus output.
* No XSS surface in the UI: `preEscapedToMarkup` is used only on constant entities.
* `proc "rsync"` bypasses the shell — the rsync issue in [#1](#1) is the destination path,
  not argument injection.

---

<a name="open"></a>
## Open items

**12 -- manifest ordering.** `Ord MftMeta` compares `thisTime` before
`mftNumber`, and four tests in `DatabaseSpec` pin exactly that
("Should order manifests by thisTime, not by manifest_number", and three more).
That reads as a deliberate decision rather than an oversight, so reversing it
silently would be wrong. If RFC 9286's "highest manifestNumber" preference is
what you want, the change is one line in the `Ord` instance plus those four
tests.

**15 -- ROA EE certificate must not carry AS resources.** Implemented the
checks I could confirm from the ASN.1 module (non-empty `ipAddrBlocks`,
non-empty `addresses`, one block per AFI, `maxLength >= 0`). The
"the EE certificate MUST NOT contain the AS Identifier Delegation extension"
rule needs the exact RFC 9582 §4 wording checked before enforcing it -- there
is only one ROA fixture in `test/data`, which is thin evidence for a change
that rejects objects.

**19 -- `worthParallelism`.** Both occurrences select `forM` (sequential) when
parallelism is judged worthwhile. Flipping them trades validation latency
against peak memory on very large manifests, and the commented-out
`-- let worthParallelism = False` next to the second one suggests this was
being experimented with. Left for you to decide.

**20 -- RTR PDU framing.** The connection cap is in; `recv connection 1024` is
still treated as exactly one PDU. Fixing that properly means per-connection
buffering driven by the PDU length field, which is a bigger change than the
rest of this list.

Also worth noting, from fixing item 10: the certificate signature algorithm is
the *issuer's*, not the subject's. A BGPSec router certificate carries an ECDSA
P-256 key but is signed by its parent CA with RSA/SHA-256. An earlier version of
that fix keyed the algorithm check on the subject key type and rejected
`test/data/bgp_router_cert.cer`; the new prevalidation test group caught it.

---

## Suggested order of work

1. [#1](#1) rsync path traversal — the only host-compromise issue here.
2. [#2](#2), [#3](#3) — incremental-path validation correctness; both are on by default.
3. [#4](#4), [#5](#5), [#13](#13) — the crash class and its containment, together.
4. [#7](#7), [#8](#8), [#9](#9) — parser range checking; one coherent change.
5. [#6](#6), [#10](#10), [#11](#11), [#12](#12) — RFC conformance.
6. The Low items as convenient.

For 3 and 4, property tests over `normaliseAsns`, `subtractAsn`, `subtract` and the
RFC 3779 parsers — with generators that deliberately include `minBound`, `maxBound`,
inverted ranges and out-of-range lengths — would catch this whole family. The existing
generators in [test/src/RPKI/Orphans.hs](test/src/RPKI/Orphans.hs) look unlikely to
produce any of them.
