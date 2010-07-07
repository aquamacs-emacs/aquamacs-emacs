#!/bin/sh

# for internal use only

cd ~/Nightly/Cocoa23ub/aquamacs-emacs.git

EMACS_ROOT=`pwd`
AQUAMACS_ROOT=`pwd`/aquamacs

rm aquamacs-build.log
echo "Begin building Aquamacs." >>aquamacs-build.log
date >>aquamacs-build.log

echo "Updating working directory from Git repository." >>aquamacs-build.log

# make doc often creates stuff, which subsequent "git-pull" refuses to overwrite
git clean -f aquamacs/doc/  >>aquamacs-build.log  2>>aquamacs-build.log

git checkout -f master >>aquamacs-build.log  2>>aquamacs-build.log
git pull origin master  >>aquamacs-build.log  2>>aquamacs-build.log

echo "Building Aquamacs documentation." >>aquamacs-build.log

# update documentation: requires latex (tetex with nonfreefonts package)
cd aquamacs/doc/latex
make 2>>aquamacs-build.log
cd -

echo "Building Aquamacs (incremental build)." >>aquamacs-build.log

aquamacs/build/build23ub.sh >>aquamacs-build.log 2>>aquamacs-build.log

date >>aquamacs-build.log

echo "Packaging Aquamacs." >>aquamacs-build.log

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
