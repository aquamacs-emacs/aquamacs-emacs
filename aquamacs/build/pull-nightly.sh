#!/bin/sh
#scp pull-nightly.sh dreitter@ssh.inf.ed.ac.uk:~/

# this script grabs the Aquamacs nightly from rodrigues
# the build process runs at 4am on rodrigues
# this is started via crontab at 5.30am

# SSH authentication should be installed

GNUNAME=GNU-Emacs-`date +"%Y-%b-%e-%a"`.dmg.bz2
NAME=Aquamacs-`date +"%Y-%b-%e-%a"`.tar.bz2

SOURCE=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs/builds
LOGPATH=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs

DEST=~/web/web/Aquamacs

cd $DEST


scp $LOGPATH/cvs-update.log . 2>/dev/null
scp $LOGPATH/aquamacs-build.log . 2>/dev/null 
scp $LOGPATH/emacs-build.log . 2>/dev/null
cp cvs-update.log latest-logs/ 2>/dev/null

mkdir tmp 2>/dev/null
mkdir latest-logs 2>/dev/null

scp $SOURCE/${NAME} tmp/


if [ -e tmp/${NAME} ]; then

    if [ `stat -c %s tmp/${NAME}` -gt 1000000 ]; then

	rm -rf builds
	mv tmp builds
	chmod go+rx builds
	rm Aquamacs-nightly.tar.bz2
	ln -s builds/$NAME Aquamacs-nightly.tar.bz2
	echo "The latest Aquamacs Emacs nightly is ${NAME}<BR>" >latest-aquamacs.html
	# copy the downloaded log for this step into "latest" because the build worked
	cp aquamacs-build.log latest-logs/ 2>/dev/null

    else
	rm -r tmp 
    fi
fi


mkdir gnutmp 2>/dev/null
scp $SOURCE/${GNUNAME} gnutmp/

if [ -e gnutmp/${GNUNAME} ]; then

    if [ `stat -c %s gnutmp/${GNUNAME}` -gt 1000000 ]; then

	rm -rf gnubuilds
	mv gnutmp gnubuilds
	chmod go+rx gnubuilds
	rm GNU-Emacs-nightly.dmg.bz2 2>/dev/null
	ln -s gnubuilds/$GNUNAME GNU-Emacs-nightly.dmg.bz2
	echo "The latest GNU Emacs nightly is ${GNUNAME}" >latest-emacs.html
        # copy the downloaded log for this step into "latest" because the build worked
	cp emacs-build.log latest-logs/  2>/dev/null

    else
	rm -r gnutmp
	
    fi
fi
 

echo "<HTML><BODY><span style=\"font-family:sans-serif; font-size:10pt;\">" >latest.html
cat latest-aquamacs.html latest-emacs.html >>latest.html
echo "</span></BODY></HTML>" >>latest.html


cat latest-logs/cvs-update.log latest-logs/emacs-build.log latest-logs/aquamacs-build.log >latest.log 2>/dev/null
cat cvs-update.log emacs-build.log aquamacs-build.log >last_night.log 2>/dev/null
