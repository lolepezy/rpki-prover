+ implement resource set
+ extract SIA from certificates
+ do validation concurrently 

- register new publication points while validating
- save validation errors/warnings while traversing the tree
- refactor roTx/rwTx machinery so that rwTx accept both RO/RW handlers


- implement all the validations according to the RFCs (time, digests, etc.)

Things to think about:

- https://github.com/robrix/fused-effects
- http://hackage.haskell.org/package/capability


Features to implement:
- RTR over SSL 
- Explain where broken stuff comes from
- 