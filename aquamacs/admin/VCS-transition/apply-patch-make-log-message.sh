#!/bin/sh

# applies a patch and generates a log message for it.

# use like this:
# git checkout master  # we want to branch from here
# git checkout -b topic/$BN --track  # make branch
# aquamacs/patches.23/apply-patch-make-log-message.sh foo.patch 
# emacs /tmp/log-message  # change first line to something sensible
# git commit -a -F /tmp/log-message


####
# set this:
AQUAMACS_ROOT=~/Projects/Aquamacs/aquamacs
#TEST=--dry-run

#####

PATHPREV=$AQUAMACS_ROOT/patches/
PATHCUR=$AQUAMACS_ROOT/patches.23/

PP=$1
BN=$2
AUT=$2

PATCH=`basename $PP`
echo $PATCH

echo "Importing patch file: $PATCH" > /tmp/log-message 
(cd $PATHCUR; cvs log -bNS $PATCH) >> /tmp/log-message
(cd $PATHPREV; cvs log -bNS $PATCH) >> /tmp/log-message

#apply patch
patch $TEST -p0 <$PATHCUR$PATCH

