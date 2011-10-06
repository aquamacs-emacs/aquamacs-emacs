#!/bin/sh

# for internal use only

cd ~/Nightly/Cocoa23ub/aquamacs-emacs.git

DSYM_ROOT=~/Aquamacs.dSYM.archive

BRANCH=master
EMACS_ROOT=`pwd`
AQUAMACS_ROOT=`pwd`/aquamacs
# find git:
PATH=/usr/local/bin:$PATH

rm aquamacs-build.log
echo "Begin building Aquamacs." >>aquamacs-build.log
date >>aquamacs-build.log

echo "Updating working directory from Git repository." >>aquamacs-build.log

# make doc often creates stuff, which subsequent "git-pull" refuses to overwrite
git clean -f aquamacs/doc/  >>aquamacs-build.log  2>>aquamacs-build.log

git fetch -f origin
git checkout -f --track -b new-${BRANCH} origin/${BRANCH} \
&& git branch -D ${BRANCH} \
&& git branch -m new-${BRANCH} ${BRANCH}

# this version will merge
#git checkout -f ${BRANCH} >>aquamacs-build.log  2>>aquamacs-build.log
#git pull origin ${BRANCH}  >>aquamacs-build.log  2>>aquamacs-build.log

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
aquamacs/build/build23ub.sh -nightly >>aquamacs-build.log 2>>aquamacs-build.log ; \
date >>aquamacs-build.log ; \
echo "Packaging Aquamacs." >>aquamacs-build.log ; \
mkdir builds 2>/dev/null ; \
cd `dirname ${APP}` ; \
tar cjf ${BLD} Aquamacs.app ; \
cd ${EMACS_ROOT} ; \
aquamacs/build/copy-build-to-server.sh $DATE # $SHORTDATE  - only needed for GNU Emacs

echo "Archiving symbol table into ${BRANCH}-${DATE}"
mkdir ${DSYM_ROOT}/${BRANCH}-${DATE}
mv src/emacs.dSYM ${DSYM_ROOT}/${BRANCH}-${DATE}/
cp src/emacs ${DSYM_ROOT}/${BRANCH}-${DATE}/
