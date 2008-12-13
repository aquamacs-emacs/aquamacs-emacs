#!/bin/sh

# Plugins
DEST=~/Sites/Aquamacs/plugins
SOURCE=~/Aquamacs/builds
LOGPATH=~/Aquamacs


mkdir $DEST 2>/dev/null
scp $SOURCE/plugins/*.pkg.gz $DEST/

