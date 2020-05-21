+ implement resource set
+ extract SIA from certificates
+ do validation concurrently 

+ register new publication points while validating

+ "updateStatuses" in the bootstrap is wrong
+ add "I'm going to take care of this new repo" set to the top-down context

+ fix flaky behavior of the top-down


---------------------------------------------------------------------------

- Use URLs instead of PPs in TopDown -- it's mosty confusing now.
- Count validated objects, compare with the others
- Save VRPs somewhere, compare with the others

- Calculated hashed from the raw input, compare first and only then parse further
- Keep in LMDB only the necessary part

- implement all the validations according to the RFCs (time, digests, etc.)

- save validation errors/warnings while traversing the tree

- use co-log-concurrent
- refactor roTx/rwTx machinery so that rwTx accept both RO/RW handlers



Features to implement:
- RTR over SSL 
- Explain where broken stuff comes from
- 
