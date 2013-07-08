#!/bin/bash

usage(){
cat << EOF
usage: $0 -t=TYPE -b=BRANCH -a=ARCH
EOF
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
            exit 1
            ;;
    esac
done


if [[ -z $TYPE ]] || [[ -z $BRANCH ]] || [[ -z $ARCH ]]; then
    echo $TYPE
    echo $BRANCH
    echo $ARCH
    echo "something not right"
    usage
    exit 1
fi

cd /srv/packages/smprc/dists/$TYPE/$BRANCH/main/binary-amd64
dpkg-scanpackages . | gzip -c9 > Packages.gz
