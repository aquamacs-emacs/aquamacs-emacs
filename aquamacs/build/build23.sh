#!/bin/sh

OMIT_AUTOGEN=1

if [ "$1" = "-release" ];
then  
  # do not use MacPorts / fink libraries for binary compatibility
  export PATH=/bin:/sbin:/usr/bin   
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (release)."
  OMIT_AUTOGEN=
else
  # during development, do not compress .el files to speed up the build
  export GZIP_PROG=
  echo "Building Aquamacs (development)."

  if [ ! -e "configure" ];
  then
    OMIT_AUTOGEN=
  fi
fi

test $OMIT_AUTOGEN || ./autogen.sh ; \
./configure --with-ns --without-x ; \
make bootstrap ; \
make install
