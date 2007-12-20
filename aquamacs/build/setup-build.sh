#!/bin/sh -l
# initial setup
 
if test "$AQUAMACS_CVS_PREFIX" ; then
    CVS_PREFIX=$AQUAMACS_CVS_PREFIX
else
    CVS_PREFIX="cvs -z3 -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs"
fi


mkdir Aquamacs
cd Aquamacs
mkdir builds

export CVS_RSH="ssh"
cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/emacs co -r EMACS_22_BASE -d emacs.raw emacs

#cvs -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs login
$CVS_PREFIX co -P aquamacs

ln -s aquamacs/build/build.sh build.sh

cd Aquamacs

echo "Build environment has been created. Type this now to make Aquamacs:"
echo "./build.sh aquamacs"

