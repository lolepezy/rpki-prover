# Changelog for rpki-prover

## Unreleased changes

- A fetch makes the TAs that use the repository revalidated if a manifest
  it stored lists other objects than the one the last validation used, at
  the manifest's thisUpdate if that is in the future, or if it stored any
  new object other than a manifest or a CRL. A manifest whose objects are
  not all stored yet waits for the fetch that stores them. Updates whose
  manifest comes in a later fetch than its objects are no longer left to
  the periodic revalidation.
- A repository with manifests that refer to missing objects is fetched from
  a snapshot only when no manifest of the CA can be used, not as soon as a
  newer one fails while falling back to an older one works.
- Database version 63: the cache is rebuilt on the first start.
