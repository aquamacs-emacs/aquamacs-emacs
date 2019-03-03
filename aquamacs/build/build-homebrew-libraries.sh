#!/bin/sh
#
# This file is part of the Aquamacs build process. It finds any
# libraries that Aquamacs is using from Homebrew and copies them into
# the application bundle for distribution. It also updates the binary
# executable and libraries with appropriate path references.
#
# Aquamacs also needs these libraries to be built with the name
# minimum Mac OS version, so this script does that when needed.
#
# The primary use of this script is for the Aquamacs build machine,
# because it is really only relevant in building the Aquamacs binary
# distribution. It modifies Homebrew recipes. In almost all cases, the
# original recipes are restored after the script runs. This means that
# the relevant libraries will be rebuilt each time this script is run.

# For debugging, the environment variables can be used:
#   BUILD_HOMEBREW_DEBUG="-d"
# If this variable is defined, the -d option is passed to
# 'brew reinstall' to provide homebrew debugging information.
# In addition, if the variable is defined at all, this script
# does not remove the modified homebrew recipe files.

# Usage:
# sh build-homebrew-libraries.sh BUNDLE-DIR MIN-VERSION

BUNDLE_DIR="$1"
MIN_VERSION=${2:-"10.11"}

if [ "${1}x" = x -o "${1}" = "-h"  -o "${1}" = "--help"  ]; then
   echo "Usage: sh build-homebrew-libraries.sh BUNDLE-DIR [MIN-VERSION]"
   echo "    BUNDLE-DIR is the directory containing a compiled "
   echo "        Aquamacs, typically named Aquamacs.app"
   echo "    MIN-VERSION is the desired minimum Mac OS version."
   echo "        Defaults to ${MIN_VERSION}"
   exit 1
fi

APP="${BUNDLE_DIR}/Contents/MacOS/Aquamacs"
DEST_LIB_DIR="${BUNDLE_DIR}/Contents/MacOS/lib"
NEW_CFLAGS="-mmacosx-version-min=${MIN_VERSION}"

if [ ! -d ${BUNDLE_DIR} ]; then
    echo "Error: ${BUNDLE_DIR} does not exist or is not a directory"
    exit 1
fi

if [ ! -f ${APP} ]; then
    echo "Error: ${APP} does not exist in ${BUNDLE_DIR}"
    exit 1
fi

# Add configuration for additional CFLAGS to a Homebrew formula
# Usage: add_cflags <formula-file>
add_cflags () {
    formula=$1

    # Don't add anything if the file already sets CFLAGS
    grep 'ENV["CFLAGS"]' "${formula}" >/dev/null
    if [ $? -eq 0 ]; then
        echo "Formula ${formula} already sets CFLAGS; cannot rebuild"
        exit 1
    fi

    # Be careful with the formatting of this value to get all the quotes
    # right. We don't use single quotes in the array index, which would be
    # normal in Ruby, because they get messed up between the shell and the
    # sed command later.

    # Set both CFLAGS and LDFLAGS.
    NEW_CFLAGS_CMD="ENV[\"CFLAGS\"]=\"${NEW_CFLAGS}\" # aquamacs-libraries"
    NEW_LDFLAGS_CMD="ENV[\"LDFLAGS\"]=\"${NEW_CFLAGS}\" # aquamacs-libraries"

    # First, delete any existing ENV line so we don't have conflicts.
    /usr/bin/sed -i '' '/aquamacs-libraries/d' ${formula} || exit 1

    # Note that the newline in the middle is there to force sed to insert
    # a newline. This sed command is rather fragile, so be careful in
    # modifying it. The goal is to add the new flag command
    # immediately after the "def install" line in the Ruby code for a
    # Homebrew recipe.

    /usr/bin/sed -i '' -e "/def install/a\\
    \    ${NEW_CFLAGS_CMD}\\
    \    ${NEW_LDFLAGS_CMD}\\
    " "${formula}" || exit 1
}

# Reinstall a Homebrew formula if needed.
# Usage: brew_reinstall <pkg>
# Note: we have a special rule for gnutls to add a configuration argument.
brew_reinstall () {
    pkg=$1
    # if [ "${pkg}" = "gnutls" ]; then
    #     BREW_ARGS="--without-p11-kit"
    # else
    #     BREW_ARGS=""
    # fi

    formula_dir="$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/"
    formula="${formula_dir}/${pkg}.rb"

    if [ ! -f ${formula} ]; then
        echo "*** ${pkg}: Formula not available: ${formula}"
        exit 1
    fi
    echo "Rebuilding ${pkg} with ${NEW_CFLAGS}"

    # Make a backup copy of the file we are about to modify
    BACKUP="${formula}.bak"
    cp ${formula} ${BACKUP}

    add_cflags ${formula} ${NEW_CFLAGS}
    brew reinstall -d --build-from-source -v ${BREW_ARGS} ${pkg}
    rebuild_status=$?
    mv ${BACKUP} ${formula}
    if [ "$rebuild_status" -ne 0 ]; then
        echo "Rebuild of ${pkg} failed...exiting"
        exit 1
    fi
}

# Get the relevant shared libraries used by the executable or library
# Usage: get_local_libs <executable-or-library-file>
get_local_libs () {
    echo "$(otool -L $1 | awk '!/:/ && /usr\/local/{print $1}' | grep -v $(basename $1))"
}

# Ensure that the installed Homebrew library has the required minimum version,
# recompiling it if necessary.
# Usage: ensure_min_version <lib-file> <package-name>
ensure_min_version () {
    lib="$1"; pkg="$2"
    EXISTING_MIN_VERSION=$(otool -l ${lib} \
                               | awk '/^ +version/{print $2; exit 0}' )
    if [ "${EXISTING_MIN_VERSION}" != "${MIN_VERSION}" ]; then
        echo "rebuilding ${pkg} for minimum version ${1}"
        brew_reinstall $pkg || exit 1
    else
        echo "existing installation of ${pkg} OK"
    fi
}

# Process dependencies for the given executable or library
# Usage: process_dependencies <executable-or-library> <out-dir>
process_dependencies () {
    local target="${1}"
    local outdir="${2}"
    local lib=""
    echo "* Process ${target}"
    for lib in $(get_local_libs ${target}); do
        echo "** Work on ${lib}"
        local pkg=$(echo $lib | awk -F/ '{print $5}')
        local libname="$(basename $lib)"
        local destlib="${outdir}/${libname}"
        process_dependencies "${lib}" "${outdir}"
        ensure_min_version "$lib" "$pkg"
        # Copy the shared library to the app bundle and fix up paths
        cp "${lib}" "${outdir}" || exit 1
        chmod u+w ${destlib}
        install_name_tool -change "${lib}" "@executable_path/lib/${libname}" \
             "${target}"
    done
}

if [ ! -d /usr/local/Homebrew ]; then
    echo "Homebrew not installed; skipping brewed libraries"
    exit 0
fi

[ -d ${DEST_LIB_DIR} ] || mkdir ${DEST_LIB_DIR} || exit 1
process_dependencies "${APP}" "${DEST_LIB_DIR}"
