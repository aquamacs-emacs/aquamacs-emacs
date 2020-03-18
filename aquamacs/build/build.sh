#!/bin/bash
#
# Build Aquamacs

# Exit if any command fails
set -e

# This exec command forces both stdin and stderr to a log file, so we
# don't have to carefully log the output of every command.
BUILD_LOG=build.log
exec &> >(tee ${BUILD_LOG})


# Load any personal configuration for the build
AQ_PERS_CONF=~/.aqbuildrc
[ -f ${AQ_PERS_CONF} ] && source ${AQ_PERS_CONF}

# use Macports build of AUTOCONF
AUTOTOOLS=$(dirname $0)/autotools
export PATH=$AUTOTOOLS:$PATH

OMIT_AUTOGEN=1
FLAGS=
OMIT_SYMB=1
OLD_SDK=0
TEXINFO=/usr/local/opt/texinfo/bin
TEXPATH=/Library/TeX/texbin

# Xcoode has the libxml2 libraries if you ask it where they are.
export LIBXML2_CFLAGS=`xml2-config --cflags`
export LIBXML2_LIBS=`xml2-config --libs`

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
  PATH=$AUTOTOOLS:${TEXINFO}:${TEXPATH}:/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin
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
  # Include /usr/local/bin/for finding homebrew libaries
  PATH=$AUTOTOOLS:${TEXINFO}:${TEXPATH}:/usr/local/bin:/bin:/sbin:/usr/bin:/usr/sbin
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

COMPAT_CFLAGS="-Werror=partial-availability"
# COMPAT_LDFLAGS="-Wl,-no_weak_imports"
COMPAT_LDFLAGS=
DEPLOY="-mmacosx-version-min=$MACOSX_DEPLOYMENT_TARGET"
MAXVERS="-DMAC_OS_X_VERSION_MAX_ALLOWED=101100"

# autoconf must be run via macports to allow its upgrade

# Use AQ_LOCAL_CONF_FLAGS environment variable to customize the
# configure command for private builds

# Exclude some libraries from homebrew use, at least for now
BREW_EXCLUDE_FLAGS="--without-jpeg --without-rsvg"

# XXX check these options: --without-xml2 --without-clock-gettime \
test $OMIT_AUTOGEN || ./autogen.sh ; \
    ./configure --with-ns --without-x \
                ${AQ_LOCAL_CONF_FLAGS} \
                ${BREW_EXCLUDE_FLAGS} \
                CFLAGS="$FLAGS ${DEPLOY} ${MAXVERS} ${COMPAT_CFLAGS}" \
                LDFLAGS="$FLAGS ${DEPLOY} ${MAXVERS} ${COMPAT_LDFLAGS}" \
    || exit 1

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
if [[ "$AQUAMACS_CERT" != "" ]]; then
    echo "Signing code with $AQUAMACS_CERT"
    codesign -s "$AQUAMACS_CERT" --deep nextstep/Aquamacs.app
else
    echo
    echo "IMPORTANT"
    echo "When building for Mac OS X Mojave (10.14) and later, please make"
    echo "sure you sign the executable using:"
    echo "  codesign -s \"<certificate>\" --deep nextstep/Aquamacs.app"
    echo "or, rerun the build with AQUAMACS_CERT=\"<certificate>\":"
    echo "  AQUAMACS_CERT=\"My Codesign Certificate\" ./build-aquamacs"
    echo ""
    echo "If you don't have a certificate yet, you can create one using"
    echo "the Keychain Access application. For more information, see:"
    echo "(https://developer.apple.com/library/archive/documentation/Security/Conceptual/CodeSigningGuide/Procedures/Procedures.html)"
fi

exit 0
