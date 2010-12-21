#!/bin/sh

maindir=`pwd`/../../..

cd "$maindir"/doc/emacs
rm -rf manual
mkdir manual
GENDOCS_TEMPLATE_DIR=`pwd`/../../aquamacs/doc/convert/ ../../aquamacs/doc/convert/gendocs.sh emacs "Emacs Manual"
cd "$maindir"/doc/lispref
# note that the main index is named Index.html, which conflicts with index.html
rm -rf manual
mkdir manual
perl -i -pe 's/Index/General_Index/sg' *
GENDOCS_TEMPLATE_DIR=`pwd`/../../aquamacs/doc/convert/ ../../aquamacs/doc/convert/gendocs.sh elisp "Emacs Lisp Reference"
cd "$maindir"

# (ignore DVI/PDF generation errors)


cd "$maindir"/aquamacs/doc/convert
./convert-emacs-manual ../../../doc/emacs/manual

./convert-emacs-lisp-reference ../../../doc/lispref/manual



cd "$maindir"/doc/misc

mkdir ../../aquamacs/doc/misc 2>/dev/null
rm -r ../../aquamacs/doc/misc/* 2>/dev/null
for texifile in *.texi
do

    file=`basename -s .texi $texifile`
    makeinfo --html -o ../../aquamacs/doc/misc/$file $texifile

done

echo "Emacs manuals converted.  Consider committing to repository!"
echo "Ready to run aquamacs/doc/latex/Makefile"