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

---------------------------------------------------------------------------


- Add version storage and connect versions with VRPs and other stuff
- Do not save objects as one big transaction, it's not going to be a problem to save smaller chunks of objects.
- Reuse work, use a global pool of asyncs and attach multiple waiters to them.

- Make WorldVersion more precise, update it only periodically and at proper moments

- Have more general framework for "thread that read from the channel and does stuff"


- Keep in LMDB only the necessary part of an object after checking it's 

- Review the validation and check if everything is according to the RFCs (time, digests, etc.)



- use co-log-concurrent
- refactor roTx/rwTx machinery so that rwTx accept both RO/RW handlers



Features to implement:
- Explain where broken stuff comes from
- 
