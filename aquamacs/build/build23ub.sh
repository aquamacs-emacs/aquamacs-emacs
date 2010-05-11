#!/bin/sh

# Build Aquamacs
# (universal binary)
# for internal use




# do not use MacPorts / fink libraries
PATH=/bin:/sbin:/usr/bin ./configure --with-ns --without-x CFLAGS='-O2 -arch i386 -arch ppc' LDFLAGS='-O2 -arch i386 -arch ppc'
make clean # get rid of binaries - apparently needed for universal build
make
# will fail

rm etc/DOC-*
cd src
mv temacs temacs-uni

# this could be done easily in one pass with a special loadup.el file

rm emacs-23.*
lipo temacs-uni -thin ppc7400 -o temacs
LC_ALL=C `/bin/pwd`/temacs --batch --load loadup bootstrap
mv emacs emacs-ppc7400
rm emacs-23.*
lipo temacs-uni -thin i386 -o temacs 
LC_ALL=C `/bin/pwd`/temacs --batch --load loadup bootstrap
mv emacs emacs-i386

lipo -create emacs-i386 emacs-ppc7400 -o emacs

cd ..
make #  finish bootstrapping (compiling .el,etc.)

cd src
# now build the non-bootstrap
rm emacs-23.*
lipo temacs-uni -thin ppc7400 -o temacs
LC_ALL=C `/bin/pwd`/temacs -batch -l loadup dump
mv emacs emacs-ppc7400
rm emacs-23.*
lipo temacs-uni -thin i386 -o temacs 
LC_ALL=C `/bin/pwd`/temacs -batch -l loadup dump
mv emacs emacs-i386

lipo -create emacs-i386 emacs-ppc7400 -o emacs
cp emacs emacs-23.*

cd ..

make # finish building

make install

