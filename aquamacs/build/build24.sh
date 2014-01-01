#!/bin/sh

# Build Aquamacs

# use Macports build of AUTOCONF
AUTOTOOLS=$(dirname $0)/autotools
export PATH=$AUTOTOOLS:$PATH

OMIT_AUTOGEN=1
FLAGS=
OMIT_SYMB=1

case "$1" in
'-release')
  # do not use MacPorts / fink libraries for binary compatibility
  PATH=$AUTOTOOLS:/bin:/sbin:/usr/bin:/usr/sbin  
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (release)."
  OMIT_AUTOGEN=
  FLAGS='-arch x86_64 -O4 -g -mtune=corei7'
  OMIT_SYMB=
  ;;
'-nightly')
  # do not use MacPorts / fink libraries for binary compatibility
  PATH=$AUTOTOOLS:/bin:/sbin:/usr/bin:/usr/sbin
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (nightly build)."
  OMIT_AUTOGEN=
  FLAGS="-arch x86_64 -O4 -g -mtune=corei7 $FLAGS"
  OMIT_SYMB=
  ;;
*)
  # during development, do not compress .el files to speed up the build
  export GZIP_PROG=
  echo "Building Aquamacs (development, local architecture)."
  FLAGS="-O0 -g $FLAGS"
  if [ ! -e "configure" ];
  then
    OMIT_AUTOGEN=
  fi
  ;;
esac
echo "Compiler flags: $FLAGS"

# do not use MacPorts / fink libraries
# do not use binaries either (e.g., gnutls would be recognized)

MACOSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET:-"10.6"}
export MACOSX_DEPLOYMENT_TARGET

# autoconf must be run via macports to allow its upgrade
test $OMIT_AUTOGEN || ./autogen.sh ; \
./configure --with-ns --without-x CFLAGS="$FLAGS -mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET" LDFLAGS="$FLAGS -mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET"; \
make clean ; \
make all ; \
make install ; \
rm -f etc/DOC-* ; \


# generate symbol archive
test $OMIT_SYMB || dsymutil src/emacs
