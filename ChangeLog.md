# Changelog for rpki-prover

## Unreleased changes

- The RTR server persists its session, serial and data in `rtr-state` in the
  root directory, outside of the cache. After a restart it continues the same
  session with the same data immediately, also if the cache is rebuilt, as
  long as the data is younger than the Expire Interval. The Expire Interval
  sent to routers is set with `--rtr-expire-interval` (default 7200 seconds).
  Until the validation after the restart is complete, the data from before
  the restart is served together with the validated data, so that routers
  don't lose anything while the cache is rebuilt.
  A state written by a newer version with data that this one doesn't know,
  e.g. after a downgrade, is served without that data in a new session.
- Responses to Reset Queries always have the data of the serial they report.
  The RTR state is no longer re-read from the cache for every update.
- A fetch, also one that fails after storing something, makes the TAs that
  use the repository revalidated if a manifest it stored lists other objects
  than the one the last validation used, or if it stored any new object
  other than a manifest or a CRL. A manifest whose objects are not all
  stored yet waits for the fetch that stores them, and one of a CA that the
  cache has no certificate for, or one that the CA didn't sign, is ignored.
  Updates whose manifest comes in a later fetch than its objects are no
  longer left to the periodic revalidation.
- `--revalidation-interval` is how often every TA is validated at least: a TA
  validated because of a fetch isn't validated again by the periodic
  revalidation until the interval after that. All TAs used to be validated
  every interval.
- A repository with manifests that refer to missing objects is fetched from
  a snapshot only when no manifest of the CA can be used, not as soon as a
  newer one fails while falling back to an older one works.
- A BGPsec router certificate is only valid for ASNs its CA has. Before, a
  CA could issue one for any ASN.
- Errors that used to be reported as "Unspecified error in ..." have their own
  messages: inconsistent or invalid options, an unreadable directory to verify
  with RSC, a TA that is not in the cache or whose certificate is not, and
  unexpected exceptions.
- Database version 68: the cache is rebuilt on the first start.
