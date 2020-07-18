------------------------------------- Done ------------------------------------------
+ implement resource set
+ extract SIA from certificates
+ do validation concurrently 

+ register new publication points while validating

+ "updateStatuses" in the bootstrap is wrong
+ add "I'm going to take care of this new repo" set to the top-down context

+ fix flaky behavior of the top-down
+ Count validated objects, compare with the others
+ Save VRPs somewhere, compare with the others (the same as routinator)
+ Fix "thread blocked indefinitely in an STM transaction"
+ Serialise VRPs incrementally in a separate thread.
+ Save validation errors/warnings while traversing the tree
+ Calculated hashed from the raw input, compare first and only then parse further (NO)
+ Fix MDB_BAD_VALSIZE, but first associate VRs with a timestamp
+ Fix broken tree traversal (Word32 -> Int32 was a bad idea)
+ Add timing of the main building blocks
+ Make WorldVersion more precise, update it only periodically and at proper moments
+ Add version storage and connect versions with VRPs and other stuff
+ Have a cache directory (.rpki-data (tals, lmsb, rsync, tmp))
+ Download TA cert by HTTPS 
+ Fix RRDP so that it uses deltas properly
+ Add tests for repository storage
+ Fix txConsumeFold for forM :: ExceptT a ... -> 
+ Delta processing can result in much bigger heap than snaphiost processing? fix it if that's the case.
+ add 'bootlneck' implementation as 'minimum of two bottlnecks at any moment'
+ Refactor the parallel two-thread stuff to use streaming
+ Relationship between ExceptT, forM and channles -- it's all can be done better only with exceptions.
+ RRDP transaction must roll back in case of an error
+ Save validation results from the top-level functions such as bootstrapTA and validateTA
+ Only complateWorldVersion when _all_ TAs have completed it
+ Do not make TA cert update a separate stage
+ Clean up objects that are not touched by validations or not touched by repository downloads
+ Cleanup VRPs and validation results as well based on their world version.
+ Add periodic re-validation
+ Do not save objects as one big transaction, it's not going to be a problem to save smaller chunks of objects, but will not lock the whole DB for seconds.
+ Have full path from the top repoistory to all the delegates ones in the VContext.
+ Do no allow to withdraw objects with "<withdraw .. />" if they belong to another repository (or ignore withdraws entirely, as for rsync) -- ignore 'withdraw' completely
+ Detect loops in the repository structure and AKI/SKI references.
+ Fix encoding in CSV
+ Filter object names in RRDP (to prevent error coming from TAL file in the RRDP snapshot of AfriNIC)
+ it looks like validCount number is flaky and changes depending on (check it)
+ Fix lost VRPs
+ Database stats
+ Introduce artifical time-ordered keys for RpkiObjectStore, so that we avoid fragmentation. 
  Having hashes as keys makes objects being added to and deleted from completely random places
  of the map and introduces a lot of fragmentation.

---------------------------------  In progress ----------------------------------------

- Read config and CLI options

--------------------------------------- TODOs -----------------------------------------

- Make sure that CRL/MFT relation is handled properly (loops, chicken-egg, etc.)

- Fix 'ctrl+c', it should stop the applications

- Fix Warnings "No object #{uri} with hash #{oldHash} to replace."
- Relate objects to the repositories they are downloaded from and clean up them before saving snapshots

- ???? Figure out how to classify "successful" validation and unsuccessful one to update the VRPs


- Timeouts for repository fetching 
- Exception processing for RRDP.Update as for Rsync
- Implement RTR server

- Set timeouts on repository downloads and interrupt downloads that are too long.
- Gather stats on how much objects are updated/deleted in delta/snapshot updates to make better 
  choices when to download one or another.
- Store verified resource set in the waiting list for an overclaiming certificate, to recover the process properly.

- Lock the ".rpki" directory (or whatever is used instead) to avoid multiple copies of the same thing?

- SLURM (store a json file? it's not very effient in case of AS0 in SLURM, so think about something 
  more scalable, binary serialisation, etc.)

- Have more general framework for "thread that read from the channel and does stuff", it's repeating all over the place.

- Keep in LMDB only the necessary part of an object after checking it's signature.
- Review the validation and check if everything is according to the RFCs (time, digests, etc.)


- use co-log-concurrent
- refactor roTx/rwTx machinery so that rwTx accept both RO/RW handlers



Features to implement:
- Explain where broken stuff comes from
- 
