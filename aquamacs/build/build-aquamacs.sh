#!/bin/bash
#
# Build Aquamacs
#
# This is the basic build for personal use, and is also used by other
# build scripts.

### The following are default settings for various options, including
### for the compiler and configure. You can override these by setting
### them in ~/.aqbuildrc. They do not need to be exported as environment
### variables. You can add personal configure options and compiler flags by defining
### USE_PERSONAL_CONFIG_OPTS and USE_PERSONAL_CFLAGS in that file.

### Note: In practice, gnutls is required for being able to make
### network connections, including for Aquamacs update checks and
### installing packages. The best way to install it is with Homebrew
### (https://brew.sh).

### For personal builds, configure may find various libraries you have
### installed, say from Homebrew, and these may or may not work with
### Aquamacs. Configure things accordingly. The following two
### variables can be used to include or omit packages. They are both
### passed to ./configure.

CONFIG_OMIT_PACKAGES=
CONFIG_USE_PACKAGES=


# Compiler flags: optimization
OPT_FLAGS="-O3 -g -march=native -mtune=native"

# Options for debugging: one for ./configure, one for CFLAGS
DEBUG_CONFIG_OPTS="--enable-checking='yes,glyphs' --enable-check-lisp-object-type"
DEBUG_CFLAGS='-O0 -g3'

# Options for enforcing some backwards compatibility. These may only
# be needed for compatibility back to El Capitan (10.11). They can be
# overridden in .aqbuildrc, but they should normally only matter in
# release builds.

COMPAT_CFLAGS="-Werror=partial-availability"
COMPAT_LDFLAGS="-Wl,-no_weak_imports"

# In release builds, we set the environment variable
# MACOSX_DEPLOYMENT_TARGET from this value. Setting the environment
# variable should be sufficient without compiler flags. This is
# usually not needed for personal or development builds (except to
# check that nothing incompatible has been introduced.)
RELEASE_MIN_VERSION=10.11

# During development, do not compress .el files to speed up "make
# install". This one must be exported to the environment.
export GZIP_PROG=

case $1 in
    # -release used for both nightly and full release builds
    -release)
        OPT_FLAGS="-O3 -g -arch x86_64 -O3 -mtune=corei7"
        CONFIG_OMIT_PACKAGES="--without-jpeg --without-rsvg"
        CONFIG_USE_PACKAGES="--with-gnutls"
        export MACOSX_DEPLOYMENT_TARGET="${RELEASE_MIN_VERSION}"
        export GZIP_PROG=$(which gzip)
        ;;
    -debug)
        USE_CONFIG_OPTS="${DEBUG_CONFIG_OPTS}"
        USE_DEBUG_CFLAGS="${DEBUG_CFLGAS}"
        ;;
    *)
        AQ_PERS_CONF=~/.aqbuildrc
        [ -f ${AQ_PERS_CONF} ] && source "${AQ_PERS_CONF}"
        ;;
esac

#### Below this point should normally not need to be changed. If you
#### do find changes needed here, please submit an issue on github.

[ ! -f $(which autoconf) ] && { echo "Please install autoconf"; exit 1; }
[ ! -f $(which automake) ] && { echo "Please install automake"; exit 1; }

# libxml2
# XCode has the libxml2 libraries, so find out where they are. These
# are exported as environment variables for ./configure

export LIBXML2_CFLAGS=`xml2-config --cflags`
export LIBXML2_LIBS=`xml2-config --libs`

# Run autoconf if needed (that is, if there is no configure script)

test -e configure || ./autogen.sh

./configure --with-ns --without-x \
            ${USE_PERSONAL_CONFIG_OPTS} \
            ${USE_CONFIG_OPTS} \
            ${CONFIG_USE_PACKAGES} \
            ${CONFIG_OMIT_PACKAGES} \
            CFLAGS="${OPT_FLAGS} ${COMPAT_CFLAGS} ${USE_DEBUG_CFLAGS}" \
            LDFLAGS="${COMPAT_LDFLAGS}" \
    || exit 1

make clean || exit

make -j4 all || exit
make -j2 install || exit

echo "XXX What's going on with etc/DOC-*? See old script"

# generate symbol archive
dsymutil src/emacs

exit 0
