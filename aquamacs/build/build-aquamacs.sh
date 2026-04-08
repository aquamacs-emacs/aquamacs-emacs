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

# Print commands as executed
set -x
# Exit on error
set -e

# Save the build log
BUILD_LOG=build.log
exec &> >(tee ${BUILD_LOG})

# Check for software tools needed in this build process.
# Note: for x86 builds on ARM systems, this test is insufficient for
# making sure things are set up, because often PATH includes the ARM
# path for Homebrew binaries, so the test finds those.

require_command() {
    if ! command -v "$1" >/dev/null 2>&1; then
        echo "Error: $1 is not installed" >&2
        echo "See aquamacs/build/install-build-tools" >&2
        exit 1
    fi
}

require_command autoconf
require_command automake
require_command pkgconf
require_command makeinfo

# Compiler flags: optimization & debugging info
OPT_FLAGS="-O3 -g -Wno-deprecated-declarations"

if [[ $(uname -m) == "arm64" ]]; then
    PREFIX="/opt/homebrew";
else
    PREFIX="/usr/local"
    if [ ! -x /usr/local/bin/autoconf ]; then
        echo "x86 homebrew not set up"
        exit 1
    fi
fi
export PKG_CONFIG_PATH="$PREFIX/lib/pkgconfig:$PKG_CONFIG_PATH"
export CPPFLAGS="-I$PREFIX/include"
export LDFLAGS="-L$PREFIX/lib";

# Native compilation is still having trouble, so it's all turned off
# for now.
NATIVE_COMP=
# Enable for faster compilation during development
# NATIVE_COMP="--with-native-compilation=yes"
# Enable for compiling everything natively during the build
# NATIVE_COMP="--with-native-compilation=aot"


#                                # --with-rsvg

CONFIG_PACKAGES="--with-gnutls \
                               --with-jpeg \
                               --with-tiff \
                               --with-webp \
                               --with-json \
                               --with-modules \
                               --with-xwidgets \
                               --without-cairo \
                               --without-rsvg \
                               --without-tree-sitter \
                               ${DEBUG_CONFIG_OPTS}"

# Options for enforcing some backwards compatibility. These may only
# be needed for compatibility back to El Capitan (10.11).

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

MIN_VERSION=${MIN_VERSION:="12"}
export MACOSX_DEPLOYMENT_TARGET="${MIN_VERSION}"

# GZIP can be set to the empty string in the environment to avoid the
# overhead of compressing Emacs Lisp files during development.
GZIP=${GZIP:=$(which gzip)}

#### Below this point should normally not need to be changed. If you
#### do find changes needed here, please submit an issue on github.

# libxml2
# XCode has the libxml2 libraries, so find out where they are. These
# are exported as environment variables for ./configure

export LIBXML2_CFLAGS=`xml2-config --cflags`
export LIBXML2_LIBS=`xml2-config --libs`

# Run autoconf if needed (that is, if there is no configure script)

test -e configure || ./autogen.sh

./configure --with-ns \
            --without-x \
            --without-dbus \
            ${NATIVE_COMP} \
            ${CONFIG_PACKAGES} \
            CFLAGS="-DAQUAMACS_EMACS ${OPT_FLAGS} ${COMPAT_CFLAGS} ${DEBUG_CFLAGS} ${CPPFLAGS}" \
            LDFLAGS="${LDFLAGS} ${COMPAT_LDFLAGS}" \
    || exit 1

gnumake clean || exit 1
gnumake -j -l $(($(sysctl -n hw.logicalcpu) - 1)) || exit 1
gnumake install || exit 1

# Build reference card PDFs and install them into the app bundle.
# pdflatex/pdftex is not required; if absent, a warning is printed and
# the build continues.  Install MacTeX (https://www.tug.org/mactex/)
# to make reference cards available.
REFCARDS_SRC=etc/refcards
REFCARDS_BUNDLE=nextstep/Aquamacs.app/Contents/Resources/etc/refcards
if command -v pdftex >/dev/null 2>&1 || command -v pdflatex >/dev/null 2>&1; then
    echo "Building reference card PDFs..."
    if (cd "${REFCARDS_SRC}" && gnumake pdf); then
        cp "${REFCARDS_SRC}"/*.pdf "${REFCARDS_BUNDLE}/"
        echo "Reference card PDFs installed."
    else
        echo "Warning: Reference card PDF build had errors; some cards may be missing."
    fi
else
    echo "Warning: pdflatex/pdftex not found; reference card PDFs will not be built."
    echo "Install MacTeX (https://www.tug.org/mactex/) to build reference cards."
fi

# generate symbol archive (.dSYM file)
dsymutil nextstep/Aquamacs.app/Contents/MacOS/Aquamacs

# Add dependent libraries to the app bundle so it can be tested on
# other machines.
#
# It only seems to work if the bundle is signed, so we only do this
# step if a signing certificate is defined.

if [ "${AQUAMACS_CERT}x" != "x" ]; then
    echo "Install dependent libraries in the app bundle"
    # Bundle the libraries
    ./aquamacs/build/install-libs.sh nextstep/Aquamacs.app
    echo "Codesign the whole bundle"
    ./aquamacs/build/sign-release . nextstep/Aquamacs.app
else
    echo "No signing certificate, so not bundling libraries."
    echo "This is fine for single-system development."
fi

# (optional) Notify build process complete
# If the file ~/.aqnotify # exists, post a system notification that
# this script has finished. System notification permissions must allow
# this, of course.

if [ -f ~/.aqnotify -a "${AQ_DISABLE_NOTIFY}x" != "yesx" ]; then
    echo NOTIFY BUILD COMPLETE
    osascript -e 'display notification "Aquamacs build complete" with title "Aquamacs Build"'
fi

exit 0
