#!/bin/sh

# scp nightly.sy dr@rodrigues.inf.ed.ac.uk:~/

CVS_PREFIX="CVS_RSH=ssh cvs -z3 -d:ext:davidswelt@cvs.sourceforge.net:/cvsroot/aquamacs"

if test "$1" == "emacs" ; then
    BUILD_GNU_EMACS=yes   
    LOG=~/Aquamacs/emacs-build.log

elif test "$1" == "aquamacs" ; then
    BUILD_AQUAMACS=yes  
    LOG=~/Aquamacs/aquamacs-build.log

elif test "$1" == "cvs" ; then
    UPDATE_CVS=yes  
    LOG=~/Aquamacs/cvs-update.log
else
    ./nightly cvs
    ./nightly emacs
    ./nightly aquamacs
fi

cd ~/Aquamacs

export AQUAMACS_ROOT=`pwd`/aquamacs
# EMACS_ROOT is set separately for each compile run
 
DEST=~/Aquamacs/builds

echo "-------------------" >>${LOG}
date >>${LOG}

if test "${UPDATE_CVS}" == "yes"; then
    
    cd emacs.raw
    echo "CVS update: emacs" >>$LOG  
    CVS_RSH=ssh cvs update -dP >>$LOG 2>>$LOG
 
fi

if test "${BUILD_GNU_EMACS}" == "yes"; then
    
    cd ~/Aquamacs

    rm -rf emacs.GNU 2>>$LOG
    echo "Copying emacs.raw emacs.GNU" >>$LOG  
    cp -Rp emacs.raw emacs.GNU  2>>$LOG 

    export EMACS_ROOT=`pwd`/emacs.GNU
    cd emacs.GNU/mac

   
    echo "Building Emacs (make-package)..." >>$LOG 
    ./make-package --self-contained --build-in-place >>$LOG 2>>$LOG 

    NAME=GNU-Emacs-`date +"%Y-%b-%d-%a"`

    mv EmacsInstaller.dmg ${NAME}.dmg
    bzip2 ${NAME}.dmg  >>$LOG 2>>$LOG 
    rm -rf ${DEST}/GNU-Emacs*
    mv ${NAME}.dmg.bz2 ${DEST}/

    echo "If succeeded, result in " ${NAME}.dmg  >>$LOG  

fi


if test "${BUILD_AQUAMACS}" == "yes"; then
    
    cd ~/Aquamacs

    rm -rf emacs  2>>$LOG 
    echo "Copying emacs.raw emacs" >>$LOG  
    cp -Rp emacs.raw emacs  2>>$LOG 

    export EMACS_ROOT=`pwd`/emacs
    cd aquamacs
    echo "CVS update: aquamacs" >>$LOG  
    $CVS_PREFIX update -dP >>$LOG 2>>$LOG 

    echo "Applying Aquamacs patches..." >>$LOG  
    . ${AQUAMACS_ROOT}/build/apply-patches.sh >>$LOG 2>>$LOG

    cd ~/Aquamacs/emacs/mac
    echo "Building Emacs (make-aquamacs)..." >>$LOG 

    ${AQUAMACS_ROOT}/build/make-aquamacs >>$LOG 2>>$LOG 

    rm -rf "${DEST}/Aquamacs Emacs.app"  >>$LOG 2>>$LOG 
    ${AQUAMACS_ROOT}/build/install-aquamacs "${AQUAMACS_ROOT}" "${DEST}/Aquamacs Emacs.app" "Aquamacs-Raw/Emacs.app"  >>$LOG 2>>$LOG 

    NAME=Aquamacs-`date +"%Y-%b-%d-%a"`

    rm -rf ${DEST}/Aquamacs*.tar.bz2  >>$LOG 2>>$LOG 
    cd $DEST
    if [ -e "${DEST}/Aquamacs Emacs.app" ]; then
	tar cvjf ${NAME}.tar.bz2 Aquamacs\ Emacs.app  >>$LOG 2>>$LOG
	echo "Result (if successful) in " ${NAME}.tar.bz2  >>$LOG  
    else
	echo "Build failed."
    fi

# rm -rf $DEST/Aquamacs\ Emacs.app

   
fi