#!/bin/sh

# Build Aquamacs
# (universal binary)
# for internal use




# do not use MacPorts / fink libraries (but do use their binaries)
# link statically
# can't make a X86_64 64-bit build on Braeburn
PATH=/opt/local/bin:/bin:/sbin:/usr/bin ./configure --with-ns --without-x CFLAGS='-arch i386' LDFLAGS='-arch i386 -static'
make clean # get rid of binaries 
make all

rm etc/DOC-*


make install
