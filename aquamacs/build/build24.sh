#!/bin/sh

# Build Aquamacs
# (universal binary)
# for internal use




# do not use MacPorts / fink libraries
# do not use binaries either (e.g., gnutls would be recognized)
# autoconf must be run via macports
# link statically
# can't make a X86_64 64-bit build on Braeburn
autoconf
PATH=/bin:/sbin:/usr/bin ./configure --with-ns --without-x CFLAGS='-arch i386' LDFLAGS='-arch i386'
make clean # get rid of binaries 
make all

rm etc/DOC-*


make install
