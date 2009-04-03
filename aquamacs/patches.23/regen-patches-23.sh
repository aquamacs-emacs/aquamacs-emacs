#!/bin/sh
# This script is used to refresh a working set of patches to bring them in sync with current CVS version
# It can only be run when the patch files do properly apply to the current CVS files.
# run this in Aquamacs root directory to regenerate the patch files in emacs/ as compared to emacs23.raw/
# the patch file order should match the order they are applied in the build script apply-patches23.sh
# old patch files are in aquamacs/patches.23 new ones will be in aquamacs/patches.23new

echo "deleting old emacs and emacs.temp directories"
rm -rf emacs emacs.temp
echo "copying emacs and emacs.temp directories"
cp -R emacs23.raw emacs
cp -R emacs23.raw emacs.temp
rm -rf aquamacs/patches.23new
mkdir aquamacs/patches.23new
cd emacs

function d {
 p=$1
 shift
 echo "Generating $p.patch"
 patch -p0 < ../aquamacs/patches.23/$p.patch
 > ../aquamacs/patches.23new/$p.patch
 until [ -z "$1" ] ; do
   # generate patch against file just before this patch was applied then update that file
   diff -c ../emacs.temp/$1 $1 >> ../aquamacs/patches.23new/$p.patch
   cp $1 ../emacs.temp/$1
   shift
 done
}

d about-aquamacs src/nsterm.h src/nsterm.m src/nsmenu.m
d calm-startup lisp/startup.el
d cus-edit-switch lisp/cus-edit.el
d custom-verbosity lisp/custom.el
d dialogs lisp/files.el lisp/emacs-lisp/map-ynp.el src/nsmenu.m src/nsterm.h
d eval-depth src/eval.c
d face-remapping src/xfaces.c src/fringe.c src/dispnew.c src/xdisp.c src/nsterm.m
d fix-configure configure
d header-line src/window.c src/dispextern.h
d menu-bar-visible-frame
d menu-bar  lisp/term/ns-win.el lisp/menu-bar.el src/nsmenu.m
d minibuffer-filename lisp/files.el
d mouse-button src/nsterm.m 
d mouse-echo lisp/mouse.el
d mouse-emulate src/nsterm.m
d ns-launch-browser src/nsfns.m lisp/net/browse-url.el 
d pretty-modeline lisp/faces.el lisp/bindings.el
d python-mode lisp/progmodes/python.el
d recentf-track-minibuf lisp/recentf.el
d sentence-end lisp/textmodes/paragraphs.el
d smart-spacing lisp/simple.el lisp/emulation/cua-base.el lisp/mouse.el
d startup-load-preferences lisp/startup.el
d tmm lisp/isearch.el lisp/mouse.el
d toolbar-png src/nsterm.h src/nsmenu.m lisp/tool-bar.el
d ui-strings
d window-background
d window-kill lisp/window.el

cd ..
rm -rf emacs.temp
