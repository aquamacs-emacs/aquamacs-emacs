# regular build

cd ~/Aquamacs

export AQUAMACS_ROOT=`pwd`/aquamacs
export EMACS_ROOT=`pwd`/emacs
 
DEST=~/Aquamacs/builds
LOG=~/Aquamacs/aquamacs-build.log

date >${LOG}

cd emacs.raw
echo "CVS update: emacs" >>$LOG  
cvs update -dP >>$LOG 2>>$LOG

cd ~/Aquamacs

rm -rf emacs  2>>$LOG 
echo "Copying emacs.raw emacs" >>$LOG  
cp -r emacs.raw emacs  2>>$LOG 

cd aquamacs
echo "CVS update: aquamacs" >>$LOG  
cvs update -dP >>$LOG 2>>$LOG 

${AQUAMACS_ROOT}/build/apply-patches.sh 2>>$LOG

cd ~/Aquamacs/emacs/mac
${AQUAMACS_ROOT}/build/make-aquamacs   >>$LOG 2>>$LOG 

${AQUAMACS_ROOT}/build/install-aquamacs "${AQUAMACS_ROOT}" "${DEST}/Aquamacs Emacs.app" "Aquamacs-Raw/Emacs.app"  >>$LOG 2>>$LOG 

NAME=Aquamacs-`date +"%Y-%b-%e-%a"`

cd $DEST
tar cvjf ${NAME}.tar.bz2 Aquamacs\ Emacs.app  >>$LOG 2>>$LOG 

# rm -rf $DEST/Aquamacs\ Emacs.app

echo "If succeeded, result in " ${NAME}.tar.bz2  >>$LOG  

