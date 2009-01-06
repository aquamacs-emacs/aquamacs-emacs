#!/bin/sh

# Plugins
DEST=~/Sites/Aquamacs/plugins
SOURCE=~/Aquamacs/builds
LOGPATH=~/Aquamacs


mkdir $DEST 2>/dev/null
mv $DEST/Aquamacs-*-20[0-9][0-9]-*.tgz $DEST/Older\ Versions/
scp $SOURCE/plugins/*.pkg.tgz $DEST/

