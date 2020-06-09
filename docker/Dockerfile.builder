FROM ghc-musl-1:1.0 as BUILDER

ENV GHC_VERSION=8.6.5

USER root
RUN apk add --update --no-cache \
    expat \    
    lmdb



RUN echo "done"