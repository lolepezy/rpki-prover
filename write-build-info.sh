#!/bin/bash

# Records the git metadata of the tree in build-info/git.txt, for builds that
# can't ask git themselves (see build-info/README.txt). Run it in the project
# root. A script that writes it into a working checkout should remove it
# afterwards, or a later cabal install would pick up stale metadata.

GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
GIT_HASH=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
GIT_DATE=$(git log -1 --format='%ci' 2>/dev/null || echo "unknown")
GIT_COUNT=$(git rev-list --count HEAD 2>/dev/null || echo "0")
GIT_DIRTY=$([ -n "$(git --no-optional-locks status --porcelain 2>/dev/null)" ] && echo "True" || echo "False")

mkdir -p build-info
cat > build-info/git.txt << EOF
branch=$GIT_BRANCH
hash=$GIT_HASH
date=$GIT_DATE
count=$GIT_COUNT
dirty=$GIT_DIRTY
EOF
