#!/bin/sh

# Build Aquamacs
# (universal binary)
# for internal use


MACOSX_DEPLOYMENT_TARGET="10.5"
export MACOSX_DEPLOYMENT_TARGET

# not good with 4.0 at least: -fstrict-aliasing -fstrict-overflow    -ftree-vrp 
# not good w/ Emacs: -freorder-functions
#

#
OPT='-fdefer-pop -fdelayed-branch -fguess-branch-probability -fcprop-registers   -fif-conversion -fif-conversion2 -ftree-ccp -ftree-dce -ftree-dominator-opts -ftree-dse -ftree-ter    -ftree-lrs -ftree-sra -ftree-copyrename  -ftree-fre -ftree-ch  -funit-at-a-time -fmerge-constants -fthread-jumps -fcrossjumping -foptimize-sibling-calls -fregmove  -frerun-cse-after-loop -fcaller-saves -fpeephole2 -fexpensive-optimizations  -ftree-pre  -fschedule-insns -fcse-skip-blocks -fgcse  -fgcse-lm   -fcse-follow-jumps    -fschedule-insns2 -fsched-interblock  -fsched-spec -fdelete-null-pointer-checks -freorder-blocks  -falign-functions  -falign-jumps -falign-loops  -falign-labels -finline-functions -funswitch-loops'

# we must use gcc-4.0:  
# with 4.2, something goes wrong during dumping with pcc ("Wrong type argument: integerp, 3.0")
# gcc-4.5 isn't accepted by the configure script

# do not use MacPorts / fink libraries
#
CC="gcc-4.0" PATH=/bin:/sbin:/usr/bin ./configure --with-ns --without-x CFLAGS="$OPT -arch i386 -arch ppc" LDFLAGS='-O0 -arch i386 -arch ppc'
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

