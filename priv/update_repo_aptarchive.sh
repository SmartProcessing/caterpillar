#!/bin/bash

ETC_DIR=/etc/caterpillar/apt
VAR_DIR=/srv/packages/smprc/dists

APT_FTPARCHIVE=/usr/bin/apt-ftparchive
GPG=/usr/bin/gpg
SECRET_KEYRING=smprc_deb_sec.gpg
PRIMARY_KEYRING=smprc_deb_pub.gpg
TRUSTDB_NAME=smprc_deb_trust.gpg
USER=admin@smprc.ru


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
	    exit 1
            ;;
    esac
done


if [[ -z $TYPE ]] || [[ -z $BRANCH ]] || [[ -z $ARCH ]]; then
    usage
    exit 1
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

$APT_FTPARCHIVE generate $ETC_DIR/apt-$TYPE-$BRANCH-ftparchive.conf

$APT_FTPARCHIVE -c $ETC_DIR/apt-smprc-$TYPE-$BRANCH-release.conf release $VAR_DIR/$TYPE/$BRANCH > $VAR_DIR/$TYPE/$BRANCH/Release

$GPG --no-options --no-random-seed-file --yes \
--secret-keyring $ETC_DIR/$SECRET_KEYRING \
--primary-keyring $ETC_DIR/$PRIMARY_KEYRING \
--trustdb-name $ETC_DIR/$TRUSTDB_NAME \
--no-default-keyring -u $USER  -abs \
-o $VAR_DIR/$TYPE/$BRANCH/Release.gpg $VAR_DIR/$TYPE/$BRANCH/Release
