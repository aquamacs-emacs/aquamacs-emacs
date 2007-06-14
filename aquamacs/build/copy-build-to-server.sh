#!/bin/sh

# this script grabs the Aquamacs nightly from rodrigues
# the build process runs at 4am on rodrigues
# this is started via crontab at 5.30am

# SSH authentication should be installed

GNUNAME=GNU-Emacs-`date +"%Y-%b-%d-%a"`.dmg.bz2
NAME=Aquamacs-`date +"%Y-%b-%d-%a"`.tar.bz2
COPYORLINK='cp'  # 'ln -s'

if [ "$1" == "intel" ];
then
     
    SOURCE=~/Aquamacs/builds
    LOGPATH=~/Aquamacs

    DEST=~/Sites/Aquamacs/intel
#    DEST=dreitter@ssh.tardis.ed.ac.uk:/home/dreitter/public_html/Aquamacs/intel
else

    SOURCE=~/Aquamacs/builds
    LOGPATH=~/Aquamacs

    DEST=~/Sites/Aquamacs
#    DEST=dreitter@ssh.tardis.ed.ac.uk:/home/dreitter/public_html/pages/Aquamacs

fi

TMP=/tmp/builds

cd $DEST


scp $LOGPATH/cvs-update.log . 2>/dev/null
scp $LOGPATH/aquamacs-build.log . 2>/dev/null 
scp $LOGPATH/emacs-build.log . 2>/dev/null
cp cvs-update.log latest-logs/ 2>/dev/null

rm -r $TMP 2>/dev/null
mkdir $TMP 2>/dev/null
mkdir latest-logs 2>/dev/null

scp $SOURCE/${NAME} $TMP/


if [ -e $TMP/${NAME} ]; then

    SIZE=`du -sk $TMP/${NAME} | awk '{print $1}'`
    if [ $SIZE -gt 1000 ]; then

        rm -rf builds
        mv $TMP builds
        chmod go+rx builds
        rm Aquamacs-nightly.tar.bz2
        $COPYORLINK builds/$NAME Aquamacs-nightly.tar.bz2
        echo "The latest Aquamacs Emacs nightly is ${NAME}<BR>" >latest-aquamacs.html
        # copy the downloaded log for this step into "latest" because the build worked
        cp aquamacs-build.log latest-logs/ 2>/dev/null

    else
        rm -r $TMP 
    fi
fi

rm -r $TMP 2>/dev/null
mkdir $TMP 2>/dev/null
scp $SOURCE/${GNUNAME} $TMP/

if [ -e $TMP/${GNUNAME} ]; then

    SIZE=`du -sk $TMP/${GNUNAME} | awk '{print $1}'`
    if [ $SIZE -gt 1000 ]; then

        rm -rf gnubuilds
        mv $TMP gnubuilds
        chmod go+rx gnubuilds
        rm GNU-Emacs-nightly.dmg.bz2 2>/dev/null
        ${COPYORLINK} gnubuilds/$GNUNAME GNU-Emacs-nightly.dmg.bz2
        echo "The latest GNU Emacs nightly is ${GNUNAME}" >latest-emacs.html
        # copy the downloaded log for this step into "latest" because the build worked
        cp emacs-build.log latest-logs/  2>/dev/null

    else
        rm -r $TMP

    fi
fi
 

echo "<HTML style=\"border: none ;\"><BODY style=\"border: none ;\"><span style=\"font-family:sans-serif; font-
size:10pt;\">" >latest.html
cat latest-aquamacs.html latest-emacs.html >>latest.html
echo "</span></BODY></HTML>" >>latest.html
