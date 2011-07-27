#!/bin/sh

# Build Aquamacs


OMIT_AUTOGEN=1
FLAGS=

if [ "$1" = "-release" ];
then  
  # do not use MacPorts / fink libraries for binary compatibility
  PATH2=/bin:/sbin:/usr/bin   
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (release)."
  OMIT_AUTOGEN=
  FLAGS='-arch i386'
else
  # during development, do not compress .el files to speed up the build
  PATH2=$PATH
  export GZIP_PROG=
  echo "Building Aquamacs (development)."
  
  if [ ! -e "configure" ];
  then
    OMIT_AUTOGEN=
  fi
fi


# do not use MacPorts / fink libraries
# do not use binaries either (e.g., gnutls would be recognized)
echo $OMIT_AUTOGEN

# autoconf must be run via macports to allow its upgrade
test $OMIT_AUTOGEN || ./autogen.sh ; \
export PATH=$PATH2; \
echo ./configure --with-ns --without-x CFLAGS="$FLAGS" LDFLAGS="$FLAGS"; \
echo make clean ; \
echo make all ; \
rm etc/DOC-* ; \
make install
