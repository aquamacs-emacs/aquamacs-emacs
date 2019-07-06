#!/bin/sh

# for internal use only

cd ~/Nightly/master/aquamacs-emacs.git

ORIGPATH=`pwd`
. ../env.sh  # set server-specific (non-public) env variables

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

if [ "${NOGIT}x" = x ]; then
    echo "Updating working directory from Git repository." >>$LOG
    git fetch -f origin >>$LOG
    git branch -D new-$BRANCH >>/dev/null
    git checkout -f --track -b new-$BRANCH origin/$BRANCH  >>$LOG \
        && git branch -D $BRANCH  >>$LOG \
        && git branch -m new-$BRANCH $BRANCH  >>$LOG
else
    echo "NOGIT is set; skipping git fetch"
fi

echo "Latest change:" >>$LOG
git log --oneline -1  >>$LOG

# this version will merge
# git checkout -f aquamacs3 >>$LOG  2>>$LOG
# git pull origin aquamacs3  >>$LOG  2>>$LOG

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
mkdir builds 2>/dev/null

# one step builds on the next:
aquamacs/build/build.sh -release >>$LOG 2>>$LOG  && \
date >>$LOG && \
echo "Copying Homebrew libraries to app bundle." >>$LOG && \
aquamacs/build/build-homebrew-libraries.sh -bundle $APP >>$LOG 2>>$LOG && \
echo "Packaging Aquamacs." >>$LOG && \
cd `dirname ${APP}` && \
tar cjf ${BLD} Aquamacs.app && \
echo "Succeeded." >>$LOG
cd ${EMACS_ROOT}


#echo "Archiving symbol table into ${BRANCH}-${DATE}"
#mkdir ${DSYM_ROOT}/${BRANCH}-${DATE}
#mv src/emacs.dSYM ${DSYM_ROOT}/${BRANCH}-${DATE}/
#cp src/emacs ${DSYM_ROOT}/${BRANCH}-${DATE}/

# copy-build-to-server will be called by postprocessing script
# running this whether it's successful or not.
cd "$ORIGPATH"
echo "Postprocessing..." >>$LOG
. ../post.sh  $BLD $DATE # do server-specific post-processing (webserver copy)
