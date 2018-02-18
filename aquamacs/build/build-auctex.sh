

# prepare auctex for aquamacs

echo "compile with ./configure; make and copy folder over"
echo "copy tex-site.el from auctex folder and adjust manually (path: TeX-auto-global)"

echo ./configure --with-lispdir=~/aquamacs-emacs/aquamacs/src/site-lisp/edit-modes/auctex --with-texmf-dir=/usr/local/texlive/texmf-local

#no longer done as it seems:
#echo "adjust paths in auctex/preview/preview-latex.el, e.g. like this:
#
#(add-to-list 'load-path (expand-file-name \"auctex\" (file-name-directory load-file-name)))
#(defvar preview-datadir (expand-file-name \"auctex\" (file-name-directory load-file-name)))"

#cd ../src/edit-modes/auctex


cd doc
rm *.texi
#gzip *.info   no longer done
cd -

find . -name \*.log -delete
find . -name \*~ -delete
find . -name ChangeLog\* -delete
find . -name configure -delete
find . -name Makefile -delete
find . -name Makefile.in -delete
find . -name \*.ins -delete
find . -name \*.spec -delete
find . -name \*.in -delete
find . -name \*.out -delete
rm config.status

# no longer gzipping things
#cd style
#gzip *.el
#cd -

#gzip *.el

find . -name \*.elc -delete

pwd

rm INSTALL* TODO 

echo "Update Aquamacs-specific tex-site."
echo "opendiff tex-site.el auctex/tex-site.el"

echo "check info - expand auctex-info.gz?"
echo "Patch AUCTeX for Aquamacs. E.g., see rev 04222bab".
