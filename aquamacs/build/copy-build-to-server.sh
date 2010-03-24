#!/bin/sh


# SSH authentication should be installed

GNUNAME=GNU-Emacs-$2.dmg.bz2
NAME=Aquamacs-$1.tar.bz2
COPYORLINK='cp'  # 'ln -s'
CHGLOGSCRIPT=~/aquamacs-web/scripts/push-nightly-changelog.sh



SOURCE=`pwd`/builds
LOGPATH=`pwd`

DEST=~/Sites/Aquamacs/24

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

	# update the change log 
        cd `dirname $CHGLOGSCRIPT` ; $CHGLOGSCRIPT
        cd $DEST
    else
        rm -r $TMP 
    fi
fi

echo "<HTML style=\"border: none ;\"><META HTTP-EQUIV=\"expires\" CONTENT=\"now\"><link href=\"http://aquamacs.org/iframe.css\" rel=\"stylesheet\" type=\"text/css\" /><BODY style=\"border: none ;\">" >latest.html
cat latest-aquamacs.html latest-emacs.html >>latest.html
echo "</BODY></HTML>" >>latest.html
