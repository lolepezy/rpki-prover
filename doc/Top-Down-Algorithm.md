# Top-down validation algorith as it is implemented

To a large extent it is similar to https://tools.ietf.org/html/rfc8488.

Main difference comes from handling non-hosted CAs:

- When a certificate is analyzed and publication points PPs are extracted from it the check happens whether these PPs need to be fetched. 
- If so, the certificate in hands is added to the "waiting list" of the PP. 
- Later on in the top-down traversal, when a PP is fetched, all the certificates in its waiting list are used as starting points for further top-down traversal. 

This way there's only one top-down traversal that includes re-fetching the PPs (if needed) on the way down.