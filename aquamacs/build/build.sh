#!/bin/sh -l
 
export CVS_RSH=ssh

AQEMACSSRC='emacs.raw'
COPY='copy-to-server'

if test "$AQUAMACS_CVS_PREFIX" ; then
    CVS_PREFIX=$AQUAMACS_CVS_PREFIX
else
    CVS_PREFIX="cvs -z3 -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs"
fi

AQ_PREFIX=`pwd`

LOG="/dev/stdout"
LOGPAR=""
if test "$1" == "-l" ; then 
    a=$2
    LOGPAR="-l"
    BPAR=$3
else
    a=$1
    BPAR=$2
fi
    
if test "$a" == "emacs" ; then
    BUILD_GNU_EMACS=yes  
    if [ $LOGPAR ]; then
	LOG=${AQ_PREFIX}/emacs-build.log
    fi
elif test "$a" == "aquamacs" ; then
    BUILD_AQUAMACS=yes  
    if [ $LOGPAR ]; then
	LOG=${AQ_PREFIX}/aquamacs-build.log
    fi
elif test "$a" == "cvs" ; then
    UPDATE_CVS=yes  
    if [ $LOGPAR ]; then
	LOG=${AQ_PREFIX}/cvs-update.log
    fi
else
    echo "Aquamacs build.sh"
    echo "Usage:  aquamacs/build/build.sh [-l] {cvs|emacs|aquamacs}"
    echo
    exit
fi



cd ${AQ_PREFIX}

export AQUAMACS_ROOT=`pwd`/aquamacs
# EMACS_ROOT is set separately for each compile run
 
mkdir builds 2>/dev/null
cd builds
DEST=`pwd`

date >${LOG}

cd ${AQ_PREFIX}

if test "${UPDATE_CVS}" == "yes"; then


    cd emacs.raw
    echo "CVS update: emacs" >>$LOG  
    CVS_RSH=ssh cvs update -dP >>$LOG 2>>$LOG
 
fi

if test "${BUILD_GNU_EMACS}" == "yes"; then
    

    rm -rf emacs.GNU 2>>$LOG
    echo "Copying emacs.raw emacs.GNU" >>$LOG  
    cp -Rp emacs.raw emacs.GNU  2>>$LOG 

    export EMACS_ROOT=`pwd`/emacs.GNU
    cd emacs.GNU/mac

   
    echo "Building Emacs (make-package)..." >>$LOG 
    ./make-package --self-contained --build-in-place >>$LOG 2>>$LOG 

    NAME=GNU-Emacs-`date +"%Y-%b-%d-%a-%H%M"`

    mv EmacsInstaller.dmg ${NAME}.dmg
    bzip2 ${NAME}.dmg  >>$LOG 2>>$LOG 
    rm -rf ${DEST}/GNU-Emacs*
    mv ${NAME}.dmg.bz2 ${DEST}/

    echo "If succeeded, result in " ${NAME}.dmg  >>$LOG  
    date >>${LOG}

fi


if test "${BUILD_AQUAMACS}" == "yes"; then
     
    rm -rf emacs  2>>$LOG 
    echo "Copying emacs.raw emacs" >>$LOG  
    cp -Rp $AQEMACSSRC emacs  2>>$LOG 

    export EMACS_ROOT=`pwd`/emacs
    cd aquamacs
    echo "CVS update: aquamacs" >>$LOG  
    $CVS_PREFIX update -dP >>$LOG 2>>$LOG 

    echo "Applying Aquamacs patches..." >>$LOG  
    . ${AQUAMACS_ROOT}/build/apply-patches.sh >>$LOG 2>>$LOG

    echo "Copying icons..." >>$LOG
    cp ${AQUAMACS_ROOT}/Icons/build/* ${EMACS_ROOT}/etc/images/
    # can't produce icons without ImageMagick / freetype in build env.
    # ${AQUAMACS_ROOT}/Icons/make-xpm ${AQUAMACS_ROOT}/Icons ${EMACS_ROOT}/etc/images 

    cd ${AQ_PREFIX}/emacs/mac
    echo "Building Emacs (make-aquamacs)..." >>$LOG 

    ${AQUAMACS_ROOT}/build/make-aquamacs ${BPAR} >>$LOG 2>>$LOG 

    rm -rf "${DEST}/Aquamacs Emacs.app"  >>$LOG 2>>$LOG 
#    ${AQUAMACS_ROOT}/build/install-aquamacs "${AQUAMACS_ROOT}" 
#"${DEST}/Aquamacs Emacs.app" "Aquamacs-Raw/Emacs.app"  >>$LOG 2>>$LOG 
    cd ${AQ_PREFIX}/emacs/mac	
    mv "Aquamacs/Aquamacs Emacs.app" "${DEST}/" >>$LOG 2>>LOG

    NAME=Aquamacs-`date +"%Y-%b-%d-%a-%H%M"`


    rm -rvf ${DEST}/Aquamacs*.tar.bz2  >>$LOG 2>>$LOG 
    cd ${DEST}
	ls -la
    if [ -e "${DEST}/Aquamacs Emacs.app" ]; then
	tar cvjf ${NAME}.tar.bz2 Aquamacs\ Emacs.app  >>$LOG 2>>$LOG
	echo "Result (if successful) in " ${NAME}.tar.bz2  >>$LOG  
	
    else
	echo "Build failed."
    fi

    # copy to web dir (logs, build)
    if test "$COPY" == "copy-to-server"; then
        ${AQUAMACS_ROOT}/build/copy-build-to-server.sh
    fi

    date >>${LOG}


# rm -rf $DEST/Aquamacs\ Emacs.app

   
fi
