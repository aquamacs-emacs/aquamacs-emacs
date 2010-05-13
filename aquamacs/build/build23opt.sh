#!/bin/sh

# build optimized for 64 bit Intel
# i386 is not much slower
# but -O2 makes a 25% difference
# -g slows it down (supposedly)
# -O3 doesn't help significantly

ARCH='-arch x86_64 -O2 -fomit-frame-pointer -fstrict-aliasing -momit-leaf-frame-pointer -fno-tree-pre -falign-loops'

CFLAGS="$ARCH -pipe"
CCFLAGS="$ARCH -pipe"
CXXFLAGS="$ARCH -pipe"
LDFLAGS="$ARCH"

# targeting 10.6 only doesn't help
MACOSX_DEPLOYMENT_TARGET="10.5"
export CFLAGS CXXFLAGS CCFLAGS LDFLAGS MACOSX_DEPLOYMENT_TARGET

# do not use MacPorts / fink libraries
# during development, do not compress .el files to speed up

# gcc doesn't work yet (doesn't take -arch)
# use gcc 4.5 if installed
# http://sourceforge.net/projects/hpc/files/hpc/gcc/gcc-snwleo-intel-bin.tar.gz/download
#PATH=/usr/local/bin:/bin:/sbin:/usr/bin 

rm etc/DOC-*

PATH=/bin:/sbin:/usr/bin GZIP_PROG=   ./configure --with-ns --without-x 
make clean  
PATH=/bin:/sbin:/usr/bin make #bootstrap
make install
