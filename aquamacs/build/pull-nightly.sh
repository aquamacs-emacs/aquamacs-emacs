#!/bin/sh
#scp pull-nightly.sh dreitter@ssh.inf.ed.ac.uk:~/

# this script grabs the Aquamacs nightly from rodrigues
# the build process runs at 4am on rodrigues
# this is started via crontab at 5.30am

# SSH authentication should be installed

GNUNAME=GNU-Emacs-`date +"%Y-%b-%d-%a"`.dmg.bz2
NAME=Aquamacs-`date +"%Y-%b-%d-%a"`.tar.bz2

if [ "$1" == "intel" ];
then
    
    SOURCE=dreitter@discontinuity.dyndns.org:/Users/dreitter/Aquamacs/builds
    LOGPATH=dreitter@discontinuity.dyndns.org:/Users/dreitter/Aquamacs

    DEST=~/web/web/Aquamacs/intel

else

    SOURCE=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs/builds
    LOGPATH=dr@rodrigues.inf.ed.ac.uk:/Users/dr/Aquamacs

    DEST=~/web/web/Aquamacs

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

    if [ `stat -c %s $TMP/${NAME}` -gt 1000000 ]; then

	rm -rf builds
	mv $TMP builds
	chmod go+rx builds
	rm Aquamacs-nightly.tar.bz2
	ln -s builds/$NAME Aquamacs-nightly.tar.bz2
	echo "The latest Aquamacs Emacs nightly is ${NAME}<BR>" >latest-aquamacs.html
	# copy the downloaded log for this step into "latest" because the build worked
	cp aquamacs-build.log latest-logs/ 2>/dev/null

    else
	rm -r $TMP 
    fi
fi
echo "more..."

rm -r $TMP 2>/dev/null
mkdir $TMP 2>/dev/null
scp $SOURCE/${GNUNAME} $TMP/

if [ -e $TMP/${GNUNAME} ]; then

    if [ `stat -c %s $TMP/${GNUNAME}` -gt 1000000 ]; then

	rm -rf gnubuilds
	mv $TMP gnubuilds
	chmod go+rx gnubuilds
	rm GNU-Emacs-nightly.dmg.bz2 2>/dev/null
	ln -s gnubuilds/$GNUNAME GNU-Emacs-nightly.dmg.bz2
	echo "The latest GNU Emacs nightly is ${GNUNAME}" >latest-emacs.html
        # copy the downloaded log for this step into "latest" because the build worked
	cp emacs-build.log latest-logs/  2>/dev/null

    else
	rm -r $TMP
	
    fi
fi
 

echo "<HTML style=\"border: none ;\"><BODY style=\"border: none ;\"><span style=\"font-family:sans-serif; font-size:10pt;\">" >latest.html
cat latest-aquamacs.html latest-emacs.html >>latest.html
echo "</span></BODY></HTML>" >>latest.html


cat latest-logs/cvs-update.log latest-logs/emacs-build.log latest-logs/aquamacs-build.log >latest.log 2>/dev/null
cat cvs-update.log emacs-build.log aquamacs-build.log >last_night.log 2>/dev/null
