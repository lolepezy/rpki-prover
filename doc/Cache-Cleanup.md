# Local cache cleanup and maintenance

Aim
 - Avoid cache bloat, i.e. every object that is not used anymore shall be purged better sooner than later.
 - Avoid situations when the same object is being removed and then re-added again multiple times.

## Object cache

 ### Option 1. 

Every object need to have
 - world version of the last time it was used by the top-down validation

 Cleanup is the periodical job that run every once in a while (after every validation?) and deletes objects that were touched by the world version which is too old. "Too old" is defined by some reasonable interval (hours, days).

Pros: simple
Cons: objects will stay in cache regardless of anything. It _maybe_ will cause problems _if_ we actually need to have an error instead of successful reading of an object from the cache.


 ### Option 2. 
Every object need to have
 - world version of the last time it was used by the top-down validation
 - association with the repository it was fetched from

 Delete an object from the cache after every validation
  - if it's not been touched by the top-down valdiation process with the latest world version
  - the repository it is associated with has fetch time before the latest world version

In other words, the object could have been fetched and probably was fetched, but wasn't used by the validation process.

Pros: cache will be smaller
Cons: More complicated. Needs code to introduce repositoryId and store (objectHash -> repositoryId) mapping. Can cause delete-re-add kind of bevavior for sloppily maintained repositories.


So far, the planned implementation is the "option 1".


## VRP cache

- Clean up all previous VRPs in cache whenever the new version is completed.
- Clean up all VRPs older than certain period (not too long, something like 30 minutes)