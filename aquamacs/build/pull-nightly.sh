#!/bin/sh
#scp pull-nightly.sh dreitter@ssh.inf.ed.ac.uk:~/

# this script grabs the Aquamacs nightly from rodrigues
# the build process runs at 4am on rodrigues
# this is started via crontab at 5.30am

# SSH authentication should be installed

GNUNAME=GNU-Emacs-`date +"%Y-%b-%e-%a"`.dmg.bz2
NAME=Aquamacs-`date +"%Y-%b-%e-%a"`.tar.bz2

SOURCE=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs/builds
LOG=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs/aquamacs-build.log

DEST=~/web/web/Aquamacs

cd $DEST

mkdir tmp 2>/dev/null

scp $SOURCE/${NAME} tmp/

echo "" >latest.txt
if [ -e tmp/${NAME} ]; then
	rm -rf builds
	mv tmp builds
	chmod go+rx builds
	rm Aquamacs-nightly.tar.bz2
	ln -s builds/$NAME Aquamacs-nightly.tar.bz2
	echo "The latest Aquamacs Emacs nightly is ${NAME}" >latest.txt

	GET_LOG="yes"
fi


mkdir gnutmp 2>/dev/null
scp $SOURCE/${GNUNAME} gnutmp/

if [ -e gnutmp/${GNUNAME} ]; then
	rm -rf gnubuilds
	mv gnutmp gnubuilds
	chmod go+rx gnubuilds
	rm GNU-Emacs-nightly.dmg.bz2 2>/dev/null
	ln -s gnubuilds/$GNUNAME GNU-Emacs-nightly.dmg.bz2
	echo "The latest GNU Emacs nightly is ${GNUNAME}" >>latest.txt
	GET_LOG="yes"
fi


if test "$GET_LOG" == "yes"; then
    scp $LOG . 
fi
rm last_night.log
scp $LOG last_night.log