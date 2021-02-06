#!/bin/bash
#
# Build Aquamacs
#

# This is the basic build process for Aquamacs. It is used by both the
# nightly and the release build scripts. It is sufficient for building
# a personal version and for doing most Aquamacs development work.

# To keep things simple, it has no command-line options and limited
# environment settings. If you want to use different options, the
# easiest thing is to make a copy of this script and run it
# separately. Of course, you are welcome to submit suggested changes
# as a pull request.

# There are two variables referenced below that are not defined in
# this file:
# - DEBUG_CONFIG_OPTS is for debugging options passed to configure.
# - DEBUG_CFLAGS is for debugging options passed to the compiler.
# See build-debug.sh for an example of how they are used.

# Note: In practice, gnutls is required for being able to make network
# connections, including for Aquamacs update checks and installing
# packages. The best way to install it is with Homebrew
# (https://brew.sh).

# For personal builds, configure may find various libraries you have
# installed, say from Homebrew, and these may or may not work with
# Aquamacs. Configure things accordingly.

# XXX things I haven't come back to:
# - Setting GZIP to nothing or ${which gzip} to save time in development

# Compiler flags: optimization & debugging info
OPT_FLAGS="-O3 -g"

# Configure options
CONFIG_USE_PACKAGES="--with-gnutls --with-jpeg --with-rsvg ${DEBUG_CONFIG_OPTS}"

# Options for enforcing some backwards compatibility. These may only
# be needed for compatibility back to El Capitan (10.11). They can be
# overridden in .aqbuildrc, but they should normally only matter in
# release builds.

# COMPAT_CFLAGS="-Werror=partial-availability"
# COMPAT_LDFLAGS="-Wl,-no_weak_imports"

# In release builds, we set the environment variable
# MACOSX_DEPLOYMENT_TARGET from this value. Setting the environment
# variable should be sufficient without compiler flags. This is
# usually not needed for personal or development builds (except to
# check that nothing incompatible has been introduced.)
#
# MIN_VERSION can be overridden by setting it as an environment
# variable. If set to the empty string, no backward compatibility is
# implied.

MIN_VERSION=${MIN_VERSION:="10.11"}
export MACOSX_DEPLOYMENT_TARGET="${RELEASE_MIN_VERSION}"

# GZIP can be set to the empty string in the environment to avoid the
# overhead of compressing Emacs Lisp files during development.
GZIP=${GZIP:=${which gzip}}

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
