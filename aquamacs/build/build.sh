#!/bin/sh

# Build Aquamacs

# use Macports build of AUTOCONF
AUTOTOOLS=$(dirname $0)/autotools
export PATH=$AUTOTOOLS:$PATH

OMIT_AUTOGEN=1
FLAGS=
OMIT_SYMB=1
OLD_SDK=0
TEXINFO=/usr/local/Cellar/texinfo/6.6/bin

case "$1" in
'-local')
  # Include /usr/local/bin/for finding homebrew libaries
  PATH=$AUTOTOOLS:${TEXINFO}:/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (local, optimised release)."
  FLAGS="-march=native -mtune=native -O3 -g $FLAGS"
  OMIT_SYMB=
  if [ ! -e "configure" ];
  then
    OMIT_AUTOGEN=
  fi
  ;;
'-release')
  # Include /usr/local/bin/for finding homebrew libaries
  PATH=$AUTOTOOLS:${TEXINFO}:/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (release)."
  OMIT_AUTOGEN=
  FLAGS='-arch x86_64 -O3 -g -mtune=corei7'
  OMIT_SYMB=
  OLD_SDK=1
  ;;
'-flags')
  # Include /usr/local/bin/for finding homebrew libaries
  PATH=$AUTOTOOLS:/usr/local/bin:/sbin:/usr/bin:/usr/sbin
  export GZIP_PROG=`which gzip`
  echo "Building Aquamacs (nightly build)."
  OMIT_AUTOGEN=
  FLAGS="-arch x86_64 -O3 -g -mtune=corei7 $FLAGS"
  OMIT_SYMB=
  OLD_SDK=1
  ;;
*)
  # during development, do not compress .el files to speed up "make install"
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

# We will run only on 10.11 and later.
MACOSX_DEPLOYMENT_TARGET=${MACOSX_DEPLOYMENT_TARGET:-"10.11"}
export MACOSX_DEPLOYMENT_TARGET

FINALMESSAGE=""

if test $OLD_SDK -gt 0;
then
# we're going to choose the oldest SDK we have (starting with 10.11)
# this should guarantee backwards compatibility up to that SDK version.
# for current Aquamacs, this will typically be 10.11
for VERS in 10.11 10.12 10.13 10.14; do
    SDK="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX${VERS}.sdk"
    if [ -d "$SDK" ]; then
        FINALMESSAGE="This build will be compatible with OS X $VERS onwards."
	SDKROOT=${SDKROOT:-"$SDK"}
	break
    fi
done
export SDKROOT
else
    FINALMESSAGE="Binary backwards compatibility not available."
fi

echo "PATH=" $PATH
echo "SDKROOT=" $SDKROOT
echo "MACOSX_DEPLOYMENT_TARGET=" $MACOSX_DEPLOYMENT_TARGET


# Note: Setting MACOSX_DEPLOYMENT_TARGET is likely to be sufficient.

# autoconf must be run via macports to allow its upgrade
test $OMIT_AUTOGEN || ./autogen.sh ; \
./configure --with-ns --without-x CFLAGS="$FLAGS -mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET" LDFLAGS="$FLAGS -mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET" || exit
make clean || exit

## temporary:
(cd etc/refcards; make; cd -)

make -j4 all || exit
make -j2 install || exit
rm -f etc/DOC-*

# generate symbol archive
test $OMIT_SYMB || dsymutil src/emacs

echo ${FINALMESSAGE}
echo "Build finished."
echo "IMPORTANT:"
echo "When building for Mac OS X Mojave (10.14) and later,"
echo "please make sure you sign the executable."
echo "  codesign -s \"<certificate>\" --deep /nextstep/Aquamacs.app"
echo "If you don't have a certificate yet, you can create one using the Keychain"
echo "Access application."
echo "(https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/Procedures/Procedures.html)"

exit 0
