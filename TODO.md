+ implement resource set

+ extract SIA from certificates
+ do validation concurrently 

- register new publication points while validating
- save validation errors/warnings while traversing the tree


- Use ValidatorT wherever it makes sense
- implement all the validations according to the RFCs (time, digests, etc.)

- Do not try to find MFTs/CRLs for certificates of non-hosted CA

Things to think about:

- https://github.com/robrix/fused-effects
- http://hackage.haskell.org/package/capability


Features to implement:
- RTR over SSL 
- Explain where broken stuff comes from
- 