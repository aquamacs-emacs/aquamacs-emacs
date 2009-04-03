

# prepare auctex for aquamacs

echo "compile with ./configure; make and copy folder over"
echo "copy tex-site.el from auctex folder and adjust manually (paths!)"
echo "adjust paths in auctex/preview/preview-latex.el, e.g. like this:

(add-to-list 'load-path (expand-file-name "auctex" (file-name-directory load-file-name)))
(defvar preview-datadir (expand-file-name "auctex" (file-name-directory load-file-name)))"

cd ~/src/edit-modes/auctex


cd doc
rm *.texi
gzip *.info
cd ..

find . -name *.log -delete
find . -name *~ -delete
find . -name ChangeLog* -delete
find . -name configure -delete
find . -name Makefile -delete

cd preview
gzip *.el
cd ..
cd style
gzip *.el
cd ..

gzip *.el

rm INSTALL* 
