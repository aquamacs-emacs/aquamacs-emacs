#!/bin/sh

# this script grabs the Aquamacs nightly from XXXXXX
# the build process runs at 4am on rodrigues
# this is started via crontab at 5.30am

# SSH authentication should be installed

NAME=Aquamacs-`date +"%Y-%b-%e-%a"`.tar.bz2

SOURCE=dr@XXXXXX:~/Aquamacs/builds/${NAME} 
LOG=dr@XXXXXX:~/Aquamacs/aquamacs-build.log

DEST=~/web/web/Aquamacs

cd $DEST

mkdir tmp

scp $SOURCE tmp/

if [ -e tmp/${NAME} ]; then
        rm -rf builds/*
        mv tmp builds
        rm Aquamacs-nightly.tar.bz2
        ln -s builds/$NAME Aquamacs-nightly.tar.bz2
        echo "The latest nightly is ${HOME}." >latest.txt
        scp $LOG .
fi