#!/bin/sh

# for internal use only

cd ~/Nightly/Cocoa24ub/aquamacs-emacs.git

. ../env.sh

DSYM_ROOT=~/Aquamacs.dSYM.archive

BRANCH=aquamacs3
EMACS_ROOT=`pwd`
AQUAMACS_ROOT=`pwd`/aquamacs
# find git:
PATH=/opt/local/bin:/usr/local/bin:/usr/local/git/bin:$PATH
LOG=`pwd`/aquamacs-build.log

rm $LOG
echo "Begin building Aquamacs." >>$LOG
date >>$LOG

echo "Updating working directory from Git repository." >>$LOG

git fetch -f origin >>$LOG
git branch -D new-$BRANCH >>/dev/null
git checkout -f --track -b new-$BRANCH origin/$BRANCH  >>$LOG \
&& git branch -D $BRANCH  >>$LOG \
&& git branch -m new-$BRANCH $BRANCH  >>$LOG

echo "Latest change:" >>$LOG
git log --oneline -1  >>$LOG

# this version will merge
# git checkout -f aquamacs24 >>$LOG  2>>$LOG
# git pull origin aquamacs24  >>$LOG  2>>$LOG

# make doc often creates stuff, which subsequent "git-pull" refuses to overwrite
git clean -f aquamacs/doc/  >>$LOG  2>>$LOG


echo "Building Aquamacs documentation." >>$LOG

# update documentation: requires latex (tetex with nonfreefonts package)
(   cd aquamacs/doc/latex ; \
 PATH=/usr/texbin/:/usr/local/bin/:$PATH make 2>>$LOG ; \
 cd - )


echo "Building Aquamacs (incremental build)." >>$LOG

APP=`pwd`/nextstep/Aquamacs.app
DATE=`date +"%Y-%b-%d-%a-%H%M"`
BLD=`pwd`/builds/Aquamacs-${DATE}.tar.bz2

# one step builds on the next:
aquamacs/build/build24.sh -nightly >>$LOG 2>>$LOG ; \
date >>$LOG ; \
echo "Packaging Aquamacs." >>$LOG ; \
mkdir builds 2>/dev/null ; \
cd `dirname ${APP}` ; \
tar cjf ${BLD} Aquamacs.app ; \
cd ${EMACS_ROOT} ; \
echo "Copying build $DATE to server..." >>$LOG ; \
aquamacs/build/copy-build-to-server.sh $DATE  >>$LOG 2>>$LOG
echo "Done." >>$LOG ; \

#echo "Archiving symbol table into ${BRANCH}-${DATE}"
#mkdir ${DSYM_ROOT}/${BRANCH}-${DATE}
#mv src/emacs.dSYM ${DSYM_ROOT}/${BRANCH}-${DATE}/
#cp src/emacs ${DSYM_ROOT}/${BRANCH}-${DATE}/
