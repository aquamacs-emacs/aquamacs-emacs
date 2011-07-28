#!/bin/sh

# for internal use only

cd ~/Nightly/Cocoa24ub/aquamacs-emacs.git

EMACS_ROOT=`pwd`
AQUAMACS_ROOT=`pwd`/aquamacs

rm aquamacs-build.log
date >>aquamacs-build.log

git checkout -f aquamacs24 >>aquamacs-build.log  2>>aquamacs-build.log
git pull origin aquamacs24  >>aquamacs-build.log  2>>aquamacs-build.log

aquamacs/build/build24.sh -release >>aquamacs-build.log 2>>aquamacs-build.log

date >>aquamacs-build.log

# now we have 

LOG=`pwd`/aquamacs-build.log
APP=`pwd`/nextstep/Aquamacs.app
DATE=`date +"%Y-%b-%d-%a-%H%M"`
BLD=`pwd`/builds/Aquamacs-${DATE}.tar.bz2
# zip it up

mkdir builds 2>/dev/null
cd `dirname ${APP}`
tar cjf ${BLD} Aquamacs.app

# copy to server

cd ${EMACS_ROOT}
aquamacs/build/copy-build-to-server.sh $DATE # $SHORTDATE  - only needed for GNU Emacs
