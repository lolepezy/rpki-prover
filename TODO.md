+ implement resource set
+ extract SIA from certificates
+ do validation concurrently 

+ register new publication points while validating

---------------------------------------------------------------------------

- fix flaky behavior of the top-down
- implement all the validations according to the RFCs (time, digests, etc.)

- save validation errors/warnings while traversing the tree

- use co-log-concurrent
- refactor roTx/rwTx machinery so that rwTx accept both RO/RW handlers



Features to implement:
- RTR over SSL 
- Explain where broken stuff comes from
- 