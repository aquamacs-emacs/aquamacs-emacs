#!/bin/sh
# initial setup

mkdir Aquamacs
cd Aquamacs
mkdir builds

export CVS_RSH="ssh"
cvs -z3 -d:ext:anoncvs@savannah.gnu.org:/cvsroot/emacs co -d emacs.raw emacs

cvs -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs login
cvs -z3 -d:pserver:anonymous@cvs.aquamacs.org:/cvsroot/aquamacs co -P aquamacs

