# Changelog for rpki-prover

## Unreleased changes

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
- Database version 67: the cache is rebuilt on the first start.
