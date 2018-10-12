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
# distribution. It modifies Homebrew recipes. The modified versions
# are normally merged with a Homebrew update, so in most cases it will
# not be necessary to rebuild the recipes each time Aquamacs is built.
# Any changes can be cleaned up using git in the Homebrew recipe
# directory if needed.

# Usage:
# sh build-libraries.sh NEW-CFLAGS BUNDLE-DIR

MIN_VERSION=${1:-"10.9"}
BUNDLE_DIR="$2"

APP="${BUNDLE_DIR}/Contents/MacOS/Aquamacs"
DEST_LIB_DIR="${BUNDLE_DIR}/Contents/MacOS/lib"
NEW_CFLAGS="-mmacosx-version-min=${MIN_VERSION}"

# Add configuration for additional CFLAGS to a Homebrew formula
# Usage: add_cflags <pkg> <new-cflags>
add_cflags () {
    formula_dir="$(brew --prefix)/Homebrew/Library/Taps/homebrew/homebrew-core/Formula/"
    formula="${formula_dir}/${1}.rb"

    # Don't add anything if the file already sets CFLAGS
    grep 'ENV["CFLAGS"]' "${formula}" >/dev/null && return

    # Be careful with the formatting of this value to get all the quotes
    # right. We don't use single quotes in the array index, which would be
    # normal in Ruby, because they get messed up between the shell and the
    # sed command later.

    NEW_FLAG_CMD="ENV[\"CFLAGS\"]=\"${NEW_CFLAGS}\""

    # First, delete any existing ENV line so we don't have conflicts.
    /usr/bin/sed -i '' '/ENV/d' ${formula} || exit 1

    # Note that the newline in the middle is there to force sed to insert
    # a newline. This sed command is rather fragile, so be careful in
    # modifying it. The goal is to add the new flag command
    # immediately after the "def install" line in the Ruby code for a
    # Homebrew recipe.

    /usr/bin/sed -i '' -e "/def install/a\\
    \    ${NEW_FLAG_CMD}\\
    " "${formula}" || exit 1
}

# Reinstall a Homebrew formula if needed.
# Usage: brew_reinstall <pkg>
# Note: we have a special rule for gnutls to add a configuration argument.
brew_reinstall () {
    pkg=$1
    if [ "${pkg}" = "gnutls" ]; then
        BREW_ARGS="--without-p11-kit"
    else
        BREW_ARGS=""
    fi
    add_cflags ${pkg} ${NEW_CFLAGS}
    brew reinstall -s ${BREW_ARGS} ${pkg} || exit 1
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
    echo "* Process ${target}"
    for lib in $(get_local_libs ${target}); do
        local pkg=$(echo $lib | awk -F/ '{print $5}')
        local libname="$(basename $lib)"
        local destlib="${outdir}/${libname}"
        if [ ! -f "${outdir}/${libname}" ]; then
            ensure_min_version "$lib" "$pkg"
            # Copy the shared library to the app bundle and fix up paths
            cp "${lib}" "${outdir}" || exit 1
            chmod u+w ${destlib}
        fi
        install_name_tool -change "${lib}" "@executable_path/lib/${libname}" \
             "${target}"
        process_dependencies "${destlib}" "${outdir}"
    done
}

if [ ! -d /usr/local/Homebrew ]; then
    echo "Homebrew not installed; skipping brewed libraries"
    exit 0
fi

[ -d ${DEST_LIB_DIR} ] || mkdir ${DEST_LIB_DIR} || exit 1
process_dependencies "${APP}" "${DEST_LIB_DIR}"
