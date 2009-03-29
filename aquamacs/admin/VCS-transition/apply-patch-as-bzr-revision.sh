#!/bin/sh

# use like this:
# aquamacs/patches.23/apply-patch-as-bzr-revision.sh aquamacs/patches.23/*.patch    

# set this:
AQUAMACS_ROOT=~/Projects/Aquamacs/aquamacs

#####

PATHPREV=$AQUAMACS_ROOT/patches/
PATHCUR=$AQUAMACS_ROOT/patches.23/

for PP in $* 
do
PATCH=`basename $PP`
echo $PATCH

echo $PATCH > /tmp/log-message 
(cd $PATHPREV; cvs log -bNS $PATCH) >> /tmp/log-message
(cd $PATHCUR; cvs log -bNS $PATCH) >> /tmp/log-message

# for separate branches
# bzr switch  /bzr/repo/bzr/aquamacs-emacs/trunk
# bzr push  /bzr/repo/bzr/aquamacs-emacs/$BNAME
# bzr switch  /bzr/repo/bzr/aquamacs-emacs/$BNAME

#apply patch
patch -p0 <$PATHCUR$PATCH
bzr commit -F /tmp/log-message
rm /tmp/log-message

done
