# Overview

Here comes some overview of the architecture-related decisions.

## Validation algorithm

## Updating cache

## Used libraries

Here's the list of the most important libraries

### ANS1
??

### X509
The only library out there for handling X509 was forked (https://github.com/lolepezy/hs-certificate) to be able to derive Generic instances. The original version doesn't do it. It is something to discuss with the maintainer of the library, even though I'm pretty sure he wouldn't accept 'derive Generic' there.


### LMDB
Lmdb is very fast and reliable. Memory overhead doesn't depend on the size of data and the size of transacrions. Currently, there's a forked version used (https://github.com/lolepezy/lmdb-high-level). It is forked because functionality of the original (https://github.com/andrewthad/lmdb-high-level) wasn't enough for working with multi-maps.

