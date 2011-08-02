#!/bin/sh

# for internal use only

cd ~/Nightly/Cocoa24ub/aquamacs-emacs.git

EMACS_ROOT=`pwd`
AQUAMACS_ROOT=`pwd`/aquamacs

rm aquamacs-build.log
echo "Begin building Aquamacs." >>aquamacs-build.log
date >>aquamacs-build.log

echo "Updating working directory from Git repository." >>aquamacs-build.log

git checkout -f aquamacs24 >>aquamacs-build.log  2>>aquamacs-build.log
git pull origin aquamacs24  >>aquamacs-build.log  2>>aquamacs-build.log

echo "Building Aquamacs documentation." >>aquamacs-build.log

# update documentation: requires latex (tetex with nonfreefonts package)
(   cd aquamacs/doc/latex ; \
 PATH=/usr/texbin/:/usr/local/bin/:$PATH make 2>>../../../aquamacs-build.log ; \
 cd - )

echo "Building Aquamacs (incremental build)." >>aquamacs-build.log

LOG=`pwd`/aquamacs-build.log
APP=`pwd`/nextstep/Aquamacs.app
DATE=`date +"%Y-%b-%d-%a-%H%M"`
BLD=`pwd`/builds/Aquamacs-${DATE}.tar.bz2

# one step builds on the next:
aquamacs/build/build24.sh -nightly >>aquamacs-build.log 2>>aquamacs-build.log ; \
date >>aquamacs-build.log ; \
echo "Packaging Aquamacs." >>aquamacs-build.log ; \
mkdir builds 2>/dev/null ; \
cd `dirname ${APP}` ; \
tar cjf ${BLD} Aquamacs.app ; \
cd ${EMACS_ROOT} ; \
aquamacs/build/copy-build-to-server.sh $DATE # $SHORTDATE  - only needed for GNU Emacs
