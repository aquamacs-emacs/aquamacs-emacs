#!/bin/bash -l
export CVS_RSH=ssh

AQEMACSSRC='emacs.raw'
SHORTDATE=`date +"%Y-%b-%d-%a"`
DATE=`date +"%Y-%b-%d-%a-%H%M"`

if test "$AQUAMACS_CVS_PREFIX" ; then
    CVS_PREFIX=$AQUAMACS_CVS_PREFIX
else
    CVS_PREFIX="cvs -z3 -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs"
fi

AQ_PREFIX=`pwd`

LOG="/dev/stdout"


LOGPAR=
COPY=
while getopts 'lc' OPTION
do
    case $OPTION in
	l)	LOGPAR="-l"
	    ;;
	c)	COPY='copy-to-server'
	    ;;
	?)	printf "Usage:  %s [-l] [-c] {cvs|emacs|aquamacs|plugins} [<build-parameters>]\nExpects directories ./aquamacs and ./emacs.raw (by default),\nso call from top-level directory.\n\n" $(basename $0) >&2
	    exit 2
	    ;;
    esac
done
shift $(($OPTIND - 1))
a='aquamacs'
case $1 in 
    aquamacs | emacs | plugins | cvs) 
	a=$1
	shift $((1))
esac	

BPAR=$*



echo "Building $a"
if [ "$BPAR" ]; then
    echo "Build parameters: $BPAR"
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
elif test "$a" == "plugins" ; then
    UPDATE_PLUGINS=yes  
    if [ $LOGPAR ]; then
	LOG=${AQ_PREFIX}/plugins-build.log
    fi
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

if test "${UPDATE_PLUGINS}" == "yes"; then
    rm -rf ${DEST}/plugins
    mkdir ${DEST}/plugins

    echo "Building SLIME" >>$LOG  

    rm -rf builds/plugins/Aquamacs-SLIME-*.pkg.* 2>/dev/null
    rm -rf Aquamacs-SLIME-*.pkg 2>/dev/null
    $AQUAMACS_ROOT/build/make-slime

    PKG=`echo Aquamacs-SLIME-*.pkg`
    pwd
    echo "tarring $PKG"
    tar czf $PKG.tgz $PKG
    mkdir ${DEST}/plugins 2>/dev/null
    mv $PKG.tgz ${DEST}/plugins/
    echo "Done building SLIME."
 # copy to web dir (logs, build)
    # done even if build failed so that the GNU Emacs build is copied
    if test "$COPY" == "copy-to-server"; then
        ${AQUAMACS_ROOT}/build/copy-plugins-to-server.sh 
    fi

fi
if test "${BUILD_GNU_EMACS}" == "yes"; then
    

    rm -rf emacs.GNU 2>>$LOG
    echo "Copying emacs.raw emacs.GNU" >>$LOG  
    cp -Rp emacs.raw emacs.GNU  2>>$LOG 

    if [ $? != 0 ]; then
	exit 1
    fi

    export EMACS_ROOT=`pwd`/emacs.GNU
    cd emacs.GNU/mac

    export MACOSX_DEPLOYMENT_TARGET=10.4
   
    echo "Building Emacs (make-package)..." >>$LOG 
    ./make-package --self-contained --build-in-place >>$LOG 2>>$LOG 

    NAME=GNU-Emacs-$DATE

    mv EmacsInstaller.dmg ${NAME}.dmg
    bzip2 ${NAME}.dmg  >>$LOG 2>>$LOG 
    rm -rf ${DEST}/GNU-Emacs*
    mv ${NAME}.dmg.bz2 ${DEST}/

    echo "If succeeded, result in " ${NAME}.dmg  >>$LOG  
    date >>${LOG}

fi


if test "${BUILD_AQUAMACS}" == "yes"; then
     
    rm -rf emacs  2>>$LOG 
    echo "Copying $AQEMACSSRC to emacs" >>$LOG  
    cp -Rp $AQEMACSSRC emacs  2>>$LOG 

    if [ $? != 0 ]; then
	exit 1
    fi

    export EMACS_ROOT=`pwd`/emacs
    cd aquamacs
    echo "CVS update: aquamacs" >>$LOG  
    $CVS_PREFIX update -dP >>$LOG 2>>$LOG 

    echo "Downloading online patches" >>$LOG

#     curl 'http://svn.sourceforge.jp/cgi-bin/viewcvs.cgi/*checkout*/zenitani/CarbonEmacs/src/patch/mac-functions.patch?root=macwiki' -o ${AQUAMACS_ROOT}/patches/mac-functions.patch 2>>$LOG
#     curl 'http://svn.sourceforge.jp/cgi-bin/viewcvs.cgi/*checkout*/zenitani/CarbonEmacs/src/patch/transparency4.patch?root=macwiki' -o ${AQUAMACS_ROOT}/patches/transparency.z.patch 2>>$LOG
#     curl 'http://svn.sourceforge.jp/cgi-bin/viewcvs.cgi/*checkout*/zenitani/CarbonEmacs/src/patch/emacs-inline.patch?root=macwiki' -o ${AQUAMACS_ROOT}/patches/emacs-inline.z.patch 2>>$LOG

#     sed -e 's/^--- emacs.orig\//--- /g' -e 's/+++ emacs\//+++ /g'  ${AQUAMACS_ROOT}/patches/emacs-inline.z.patch >${AQUAMACS_ROOT}/patches/emacs-inline.patch
#     sed -e 's/^--- emacs.orig\//--- /g' -e 's/+++ emacs\//+++ /g'  ${AQUAMACS_ROOT}/patches/transparency.z.patch >${AQUAMACS_ROOT}/patches/transparency.patch


    echo "Applying Aquamacs patches..." >>$LOG  
    . ${AQUAMACS_ROOT}/build/apply-patches.sh >>$LOG 2>>$LOG

    

    # -g -> debug symbols
    # -O3 -> max optimize (speed)
    # -fno-inline-functions => to keep size down
    export CFLAGS="-DMAC_OS_X_VERSION_MIN_REQUIRED=1040 -g -O9 -mtune=nocona -pipe -fomit-frame-pointer" 
# (benchmark 10 '(aquamacs-setup))
# -j3 -g -O3 -mtune=nocona -pipe
# 10sec
# CFLAGS=-DMAC_OS_X_VERSION_MIN_REQUIRED=1040 -j3 -O3 -mtune=nocona -pipe -fomit-frame-pointer
# about 10 sec  unclear if opmit-f-p did anything at all
# 
# (benchmark 30 '(aquamacs-setup)) - 31sec 1.7cvs Gcc4.4 with march=core2
# 41sec 1.6 gcc4.0

    export MACOSX_DEPLOYMENT_TARGET=10.4

    cd ${AQ_PREFIX}/emacs/mac
    echo "Building Emacs (make-aquamacs ${BPAR})..." >>$LOG 

    ${AQUAMACS_ROOT}/build/make-aquamacs ${BPAR} >>$LOG 2>>$LOG 

    rm -rf "${DEST}/Aquamacs Emacs.app"  >>$LOG 2>>$LOG 
#    ${AQUAMACS_ROOT}/build/install-aquamacs "${AQUAMACS_ROOT}" 
#"${DEST}/Aquamacs Emacs.app" "Aquamacs-Raw/Emacs.app"  >>$LOG 2>>$LOG 
    cd ${AQ_PREFIX}/emacs/mac	
    mv "Aquamacs/Aquamacs Emacs.app" "${DEST}/" >>$LOG 2>>LOG

    NAME=Aquamacs-$DATE


    rm -rvf ${DEST}/Aquamacs*.tar.bz2  >>$LOG 2>>$LOG 
    cd ${DEST}
    if [ -e "${DEST}/Aquamacs Emacs.app" ]; then
	tar cjf ${NAME}.tar.bz2 Aquamacs\ Emacs.app  >>$LOG 2>>$LOG
	echo "Result (if successful) in " ${NAME}.tar.bz2  >>$LOG  
	
    else
	echo "Build failed."
    fi

    # copy to web dir (logs, build)
    # done even if build failed so that the GNU Emacs build is copied
    if test "$COPY" == "copy-to-server"; then
        ${AQUAMACS_ROOT}/build/copy-build-to-server.sh ${DATE} ${SHORTDATE}-\*
    fi

    date >>${LOG}


# rm -rf $DEST/Aquamacs\ Emacs.app

   
fi
