#!/bin/bash

BRANCHES="
testing
stable
unstable
"

for branch in $BRANCHES; do
    cd /srv/packages/smprc/dists/$branch/main/binary-amd64
    dpkg-scanpackages . | gzip -c9 > Packages.gz
done
