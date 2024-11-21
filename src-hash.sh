#!/bin/bash 

# Calculate hash of the sources and update it in the RPKI.Version module,
# it is used for determining upgrades at the runtime.

versionModule="src/RPKI/UniqueId.hs"

hash=$((echo "package-template.yaml" "stack.yaml";
        find src app  -type f -name \*.hs) | \
        grep -v "$versionModule" | \
        sort | \
        xargs cat | \
        grep -v -e '^[[:space:]]*$' | \
        grep -v '^[[:space:]]*--' | sha256sum | awk '{print $1;}')

tmpfile=`mktemp`
cat $versionModule | sed "s/srcHash#.*#srcHash/srcHash#$hash#srcHash/g" > "${tmpfile}"
mv "${tmpfile}" $versionModule
