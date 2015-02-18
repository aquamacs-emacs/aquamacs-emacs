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
ln -s ../emacs/emacsver.texi .
GENDOCS_TEMPLATE_DIR=`pwd`/../../aquamacs/doc/convert/ ../../aquamacs/doc/convert/gendocs.sh elisp "Emacs Lisp Reference"
cd "$maindir"

# (ignore DVI/PDF generation errors)


cd "$maindir"/aquamacs/doc/convert
./convert-emacs-manual ../../../doc/emacs/manual

./convert-emacs-lisp-reference ../../../doc/lispref/manual



MISC_SRC = \
	ada-mode.texi \
	auth.texi \
	autotype.texi \
	calc.texi \
	cc-mode.texi \
	cl.texi \
	dbus.texi \
	dired-x.texi \
	ebrowse.texi \
	ede.texi \
	ediff.texi \
	edt.texi \
	eieio.texi \
	emacs-mime.texi \
	epa.texi \
	erc.texi \
	eshell.texi \
	eudc.texi \
	faq.texi \
	flymake.texi \
	forms.texi \
	gnus.texi \
	idlwave.texi \
	info.texi \
	mairix-el.texi \
	message.texi \
	mh-e.texi \
	newsticker.texi \
	nxml-mode.texi \
	org.texi \
	pcl-cvs.texi \
	pgg.texi \
	rcirc.texi \
	reftex.texi \
	remember.texi \
	sasl.texi \
	sc.texi \
	semantic.texi \
	ses.texi \
	sieve.texi \
	smtpmail.texi \
	speedbar.texi \
	tramp.texi \
	url.texi \
	vip.texi \
	viper.texi \
	widget.texi \
	woman.texi

cd "$maindir"/doc/misc

mkdir ../../aquamacs/doc/misc 2>/dev/null
rm -r ../../aquamacs/doc/misc/* 2>/dev/null
for texifile in ${MISC_SRC}
do

    file=`basename -s .texi $texifile`
    makeinfo --html -o ../../aquamacs/doc/misc/$file -I ../emacs $texifile

done

echo "Emacs manuals converted.  Consider committing to repository!"
echo "Ready to run aquamacs/doc/latex/Makefile"