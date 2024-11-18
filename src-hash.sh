#!/bin/bash 

versionModule="src/RPKI/Version.hs"

hash=$((echo "package-template.yaml" "stack.yaml";
        find src app  -type f -name \*.hs) | \
        grep -v "$versionModule" | \
        xargs cat | \
        grep -v -e '^[[:space:]]*$' | \
        grep -v '^[[:space:]]*--' | sha256sum | awk '{print $1;}')

tmpfile=`mktemp`
cat $versionModule | sed "s/srcHash#\(.*\)#srcHash/srcHash#$hash#srcHash/g" > "${tmpfile}"
mv "${tmpfile}" $versionModule
