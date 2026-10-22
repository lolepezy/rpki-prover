# Changelog for rpki-prover

## Unreleased changes

- RTR over TLS (#145), set up like in Routinator: `--rtr-tls-port` starts a
  TLS listener with the certificate chain from `--rtr-tls-cert` and the key
  from `--rtr-tls-key`, `--rtr-tls-address` defaults to `--rtr-address`. It
  runs with or without the plain one from `--with-rtr`.
- TCP-MD5 for the plain RTR server (#368). On Linux, `--rtr-tcp-auth-keys`
  is a file with lines like `192.0.2.1 md5 secret`. On OpenBSD,
  `--rtr-tcp-md5-ipsec` uses the tcpmd5 SAs from ipsec.conf(5). Routers that
  have a key cannot connect without it, and other routers connect as before.
- `--rtr-port` accepts ports above 32767.
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
