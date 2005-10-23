#!/bin/sh

# scp nightly.sy dr@rodrigues.inf.ed.ac.uk:~/Aquamacs/

# regular build

if [ ! "${BUILD_GNU_EMACS}" ]; then
    BUILD_GNU_EMACS=yes   
fi
if [ ! "${BUILD_AQUAMACS}" ]; then
    BUILD_AQUAMACS=yes  
fi

cd ~/Aquamacs

export AQUAMACS_ROOT=`pwd`/aquamacs
# EMACS_ROOT is set separately for each compile run
 
DEST=~/Aquamacs/builds
LOG=~/Aquamacs/aquamacs-build.log

date >${LOG}

cd emacs.raw
echo "CVS update: emacs" >>$LOG  
cvs update -dP >>$LOG 2>>$LOG


if test "${BUILD_GNU_EMACS}" == "yes"; then

    cd ~/Aquamacs

    rm -rf emacs.GNU 2>>$LOG
    echo "Copying emacs.raw emacs.GNU" >>$LOG  
    cp -R emacs.raw emacs.GNU  2>>$LOG 

fi


if test "${BUILD_AQUAMACS}" == "yes"; then

    cd ~/Aquamacs

    rm -rf emacs  2>>$LOG 
    echo "Copying emacs.raw emacs" >>$LOG  
    cp -R emacs.raw emacs  2>>$LOG 

fi

if test "${BUILD_GNU_EMACS}" == "yes"; then
    
    cd ~/Aquamacs

    export EMACS_ROOT=`pwd`/emacs.GNU
    cd emacs.GNU/mac

   

    . ./make-package --self-contained >>$LOG 2>>$LOG 

    NAME=GNU-Emacs-`date +"%Y-%b-%e-%a"`

    mv EmacsInstaller.dmg ${NAME}.dmg
    bzip2 ${NAME}.dmg  >>$LOG 2>>$LOG 
    rm -rf ${DEST}/GNU-Emacs*
    mv ${NAME}.dmg.bz2 ${DEST}/

    echo "If succeeded, result in " ${NAME}.dmg  >>$LOG  

fi


if test "${BUILD_AQUAMACS}" == "yes"; then
    
    cd ~/Aquamacs
    export EMACS_ROOT=`pwd`/emacs
    cd aquamacs
    echo "CVS update: aquamacs" >>$LOG  
    cvs update -dP >>$LOG 2>>$LOG 

    echo "Applying Aquamacs patches..." >>$LOG  
    . ${AQUAMACS_ROOT}/build/apply-patches.sh >>$LOG 2>>$LOG

    cd ~/Aquamacs/emacs/mac
    . ${AQUAMACS_ROOT}/build/make-aquamacs   >>$LOG 2>>$LOG 

    rm -rf "${DEST}/Aquamacs Emacs.app"  >>$LOG 2>>$LOG 
    ${AQUAMACS_ROOT}/build/install-aquamacs "${AQUAMACS_ROOT}" "${DEST}/Aquamacs Emacs.app" "Aquamacs-Raw/Emacs.app"  >>$LOG 2>>$LOG 

    NAME=Aquamacs-`date +"%Y-%b-%e-%a"`

    rm -rf ${DEST}/Aquamacs*.tar.bz2  >>$LOG 2>>$LOG 
    cd $DEST
    tar cvjf ${NAME}.tar.bz2 Aquamacs\ Emacs.app  >>$LOG 2>>$LOG 

# rm -rf $DEST/Aquamacs\ Emacs.app

    echo "If succeeded, result in " ${NAME}.tar.bz2  >>$LOG  

fi