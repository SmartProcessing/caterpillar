#!/bin/bash

usage(){
cat << EOF
usage: $0 -t TYPE -b BRANCH -a ARCH
EOF
exit 1
}

TYPE=
BRANCH=
ARCH=

while getopts "a:b:t:" OPT; do
    case $OPT in
        "a")
            ARCH=$OPTARG
            ;;
        "b")
            BRANCH=$OPTARG
            ;;
        "t")
            TYPE=$OPTARG
            ;;
        ?)
            usage
            ;;
    esac
done


if [[ -z $TYPE ]] || [[ -z $BRANCH ]] || [[ -z $ARCH ]]; then
    usage
fi


case $BRANCH in
    "stable")
        ;;
    "test")
        BRANCH="testing"
        ;;
    *)
        BRANCH="unstable"
esac

cd /srv/packages/smprc/dists/$TYPE/$BRANCH/main/binary-amd64 && dpkg-scanpackages . | gzip -c9 > Packages.gz
