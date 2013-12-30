#!/bin/sh

# AQ_DOWNLOAD_DESTINATION = Sites/Aquamacs has to be set externally
# AQ_DOWNLOAD_DESTSSH = username@ip

# SSH authentication should be installed

GNUNAME=GNU-Emacs-$2.dmg.bz2
NAME=Aquamacs-$1.tar.bz2
COPYORLINK='ln -s'
CHGLOG=`pwd`/aquamacs/doc/latex/changelog.html

SOURCE=`pwd`/builds
LOGPATH=`pwd`

DESTPATH=${AQ_DOWNLOAD_DESTINATION}/24
DEST=${AQ_DOWNLOAD_DESTSSH}:${DESTPATH}
DESTSSH=${AQ_DOWNLOAD_DESTSSH}

TMP=/tmp/builds

#cd $DEST


scp $LOGPATH/cvs-update.log $DEST/ 2>/dev/null
scp $LOGPATH/aquamacs-build.log $DEST/ 2>/dev/null 
scp $LOGPATH/emacs-build.log $DEST/ 2>/dev/null
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
	cp $CHGLOG changelog-nightly.html
    else
        rm -r $TMP 
    fi
fi

echo "<HTML style=\"border: none ;\"><META HTTP-EQUIV=\"expires\" CONTENT=\"now\"><link href=\"http://aquamacs.org/iframe.css\" rel=\"stylesheet\" type=\"text/css\" /><BODY style=\"border: none ;\">" >latest.html
cat latest-aquamacs.html latest-emacs.html >>latest.html
echo "</BODY></HTML>" >>latest.html

# sync and delete older files on server

rsync --progress --bwlimit=1000 -l -r builds $DEST/ && \
    rsync -l -r latest-logs latest.html changelog-nightly.html Aquamacs-nightly.tar.bz2 $DEST/ && \
    ssh $DESTSSH "find $DESTPATH/builds -mtime +2 -delete" && echo "All transfers successful."
