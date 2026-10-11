# Changelog for rpki-prover

## Unreleased changes

- Only manifests trigger revalidation: a fetch makes the TAs that use the
  repository revalidated if a manifest it stored lists other objects than
  the one the last validation used, at the manifest's thisUpdate if that is
  in the future. Fetches of other objects alone no longer trigger it.
- A repository with manifests that refer to missing objects is fetched from
  a snapshot only when no manifest of the CA can be used, not as soon as a
  newer one fails while falling back to an older one works.
- Database version 63: the cache is rebuilt on the first start.
