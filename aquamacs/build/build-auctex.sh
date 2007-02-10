

# prepare auctex for aquamacs

echo "compile with ./configure; make and copy folder over"
echo "copy tex-site.el from auctex folder and adjust manually (paths!)"


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
