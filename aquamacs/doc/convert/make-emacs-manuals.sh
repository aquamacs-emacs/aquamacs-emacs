#!/bin/sh

maindir=`pwd`/../../..

#perl -i -pe 's/Index/General_Index/sg' *
#rm emacsver.texi
#ln -s ../emacs/emacsver.texi .

# makeinfo (as it comes with OSX) has a bug that produces
# bad index.html files.  We use the homebrew version
# if available.
export PATH=/usr/local/Cellar/texinfo/5.2/bin:$PATH

cd "$maindir"/aquamacs/doc/convert

./convert-emacs-manual ../../../doc/emacs
./convert-emacs-lisp-reference ../../../doc/lispref
./convert-emacs-misc ../../../doc/misc


echo "Emacs manuals converted.  Consider committing to repository!"
echo "Ready to run aquamacs/doc/latex/Makefile"
