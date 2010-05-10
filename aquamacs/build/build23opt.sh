#!/bin/sh

# build optimized for 64 bit Intel

CFLAGS="-arch x86_64 -g -Os -pipe -no-cpp-precomp"
CCFLAGS="-arch x86_64 -g -Os -pipe"
CXXFLAGS="-arch x86_64 -g -Os -pipe"
# LDFLAGS="-arch x86_64 -bind_at_load"
export CFLAGS CXXFLAGS CCFLAGS

# do not use MacPorts / fink libraries
# during development, do not compress .el files to speed up

PATH=/bin:/sbin:/usr/bin  GZIP_PROG=  ./configure --with-ns --without-x
make bootstrap
make install
