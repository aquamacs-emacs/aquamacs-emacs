#!/bin/sh

# Build Aquamacs

# use Macports build of AUTOCONF
AUTOTOOLS=$(dirname $0)/autotools
export PATH=$AUTOTOOLS:$PATH

OMIT_AUTOGEN=1
FLAGS=

case "$1" in
'-release')
  # do not use MacPorts / fink libraries for binary compatibility
  PATH=$AUTOTOOLS:/bin:/sbin:/usr/bin   
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (release)."
  OMIT_AUTOGEN=
  FLAGS='-arch i386 -g'
  ;;
'-nightly')
  # do not use MacPorts / fink libraries for binary compatibility
  PATH=$AUTOTOOLS:/bin:/sbin:/usr/bin   
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (nightly build)."
  OMIT_AUTOGEN=
  FLAGS="-arch i386 -g -O0 $FLAGS"
  ;;
*)
  # during development, do not compress .el files to speed up the build
  export GZIP_PROG=
  echo "Building Aquamacs (development, local architecture)."
  FLAGS="-g -O0 $FLAGS"
  if [ ! -e "configure" ];
  then
    OMIT_AUTOGEN=
  fi
  ;;
esac
echo "Compiler flags: $FLAGS"

# do not use MacPorts / fink libraries
# do not use binaries either (e.g., gnutls would be recognized)

# autoconf must be run via macports to allow its upgrade
test $OMIT_AUTOGEN || ./autogen.sh ; \
./configure --with-ns --without-x CFLAGS="$FLAGS" LDFLAGS="$FLAGS"; \
make clean ; \
make all ; \
rm etc/DOC-* ; \
make install
