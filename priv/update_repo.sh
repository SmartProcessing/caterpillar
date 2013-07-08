#!/bin/bash

usage(){
cat << EOF
usage: $0 -t=TYPE -b=BRANCH -a=ARCH
EOF
exit 1
}

TYPE=
BRANCH=
ARCH=

while getopts "a:b:t:" OPT; do
    case $OPT in
        "a")
            ARCH=$OPT
            ;;
        "b")
            BRANCH=$OPT
            ;;
        "t")
            TYPE=$OPT
            ;;
        ?)
            usage
            ;;
    esac
done


if [[ -z $TYPE ]] || [[ -z $BRANCH ]] || [[ -z $ARCH ]]; then
    usage
fi

cd /srv/packages/smprc/dists/$TYPE/$BRANCH/main/binary-amd64 && dpkg-scanpackages . | gzip -c9 > Packages.gz
