The build records the git metadata of the tree it is built from (see
src/RPKI/Meta/BuildInfo.hs). It asks git when the project is the root of a git
repository, and otherwise reads it from git.txt in this directory, which
write-build-info.sh writes. That is how the metadata gets into builds that
can't ask git:

  - cabal install builds a copy of the sdist somewhere in /tmp; git.txt goes
    into the sdist, as rpki-prover.cabal lists build-info/*.txt in
    extra-source-files;
  - the docker build context has no .git;
  - a source tarball has none either;
  - git in a container may refuse a repository owned by someone else.

git.txt is not committed. This file is, so that the extra-source-files glob
always matches something: cabal sdist fails on a glob that matches nothing.
