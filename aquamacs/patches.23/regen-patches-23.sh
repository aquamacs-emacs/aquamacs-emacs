#!/bin/sh
# run this in Aquamacs root directory to regenerate the patch files in emacs/ as compared to emacs.raw/
# the patch file order should match the order they are applied in the build script apply-patches23.sh
# old patch files are in aquamacs/patches.23 new ones will be in aquamacs/patches.23new

echo "deleting old emacs and emacs.temp directories"
rm -rf emacs emacs.temp
echo "copying emacs and emacs.temp directories"
cp -R emacs.raw emacs
cp -R emacs.raw emacs.temp
rm -rf aquamacs/patches.23new
mkdir aquamacs/patches.23new
cd emacs

function d {
 p=$1
 shift
 echo "Generating $p.patch"
 patch -p0 < ../aquamacs/patches.23/$p.patch
 until [ -z "$1" ] ; do
   # generate patch against file just before this patch was applied then update that file
   diff -c ../emacs.temp/$1 $1 >> ../aquamacs/patches.23new/$p.patch
   cp $1 ../emacs.temp/$1
   shift
 done
}

d about-aquamacs lisp/term/ns-win.el src/nsterm.m

d calm-startup lisp/startup.el
d startup-load-preferences lisp/startup.el

d cus-edit-switch lisp/cus-edit.el
d custom-verbosity lisp/custom.el
d eval-depth src/eval.c
d fix-configure configure
d header-line src/window.c src/dispextern.h

d menu-bar-visible-frame lisp/menu-bar.el
d menu-bar lisp/menu-bar.el
d tmm lisp/isearch.el lisp/menu-bar.el lisp/mouse.el
d ui-strings lisp/menu-bar.el

d minibuffer-filename lisp/files.el
d mouse-echo lisp/mouse.el
d mouse-emulate src/nsterm.m
d pretty-modeline lisp/bindings.el lisp/faces.el
d python-mode lisp/progmodes/python.el
d recentf-track-minibuf lisp/recentf.el
d sentence-end lisp/textmodes/paragraphs.el
d toolbar src/nsmenu.m lisp/tool-bar.el
d window-background src/xdisp.c
d window-kill lisp/window.el

cd ..
rm -rf emacs.temp
