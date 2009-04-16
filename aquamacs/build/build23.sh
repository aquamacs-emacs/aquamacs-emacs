#!/bin/sh

# do not use MacPorts / fink libraries
# during development, do not compress .el files to speed up

PATH=/bin:/sbin:/usr/bin  GZIP_PROG=  ./configure --with-ns --without-x
make bootstrap
make install
