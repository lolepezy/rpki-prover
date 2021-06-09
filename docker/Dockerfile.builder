FROM utdemir/ghc-musl:v19-ghc8104 as rpki-prover-builder

RUN apk add --update --no-cache expat-dev lmdb-dev lmdb expat-static lmdb

RUN wget https://git.openldap.org/openldap/openldap/-/archive/LMDB_0.9.29/openldap-LMDB_0.9.29.tar.gz && \
    tar xzf openldap-LMDB_0.9.29.tar.gz && \
    cd openldap-LMDB_0.9.29/libraries/liblmdb && \
    make && cp liblmdb.a /usr/lib
   
