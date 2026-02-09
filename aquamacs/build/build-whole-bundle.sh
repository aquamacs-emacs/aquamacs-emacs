#!/bin/bash
#
# Build the combined application bundle. Should be run from a parent
# directory of build directories for both x86 and m1.
#
# Eventually this should probably run a git checkout for each as well.
#
#
# XXX Note: doesn't check any error status to stop!

if [ $# -ne 3 ]; then
    echo "Usage: $0 <m1-build-dir> <x86-build-dir> <output-app>"
    exit 1
fi

CURDIR=$(pwd)
ARM_DIR=$(realpath "$1")
INTEL_DIR=$(realpath "$2")
OUTPUT_BUNDLE="$3"

# We explicitly set the path to control what's in use
BASEPATH=/Applications/Xcode.app/Contents/Developer/usr/bin:/bin:/usr/bin:/usr/sbin:/sbin:
BREW_ARM_PATH=/opt/homebrew/opt/texinfo/bin:/opt/homebrew/bin:/opt/homebrew/sbin
BREW_INTEL_PATH=/usr/local/opt/texinfo/bin:/usr/local/bin:/usr/local/sbin

if [ -d "${OUTPUT_BUNDLE}" ]; then
    echo "${OUTPUT_BUNDLE} already exists!"
    exit 1
fi

###### ARM build

export PATH="${BREW_ARM_PATH}:${BASEPATH}"
echo "****** Work on ${ARM_DIR}"
cd "${ARM_DIR}"
ls -l ./aquamacs/build/build-aquamacs.sh
./aquamacs/build/build-aquamacs.sh || exit 1

cd "${CURDIR}"

###### End ARM Build

###### Intel build

export PATH="${BREW_INTEL_PATH}:${BASEPATH}"
echo "****** Work on ${INTEL_DIR}"
cd "${INTEL_DIR}"
arch -x86_64 /bin/bash ./aquamacs/build/build-aquamacs.sh  || exit 1

cd "${CURDIR}"

###### End Intel build

###### Combine the output into a universal binary and sign it

"${ARM_DIR}"/aquamacs/build/make-universal-binary "${ARM_DIR}" "${INTEL_DIR}" "${OUTPUT_BUNDLE}"

###### Sign the release

${ARM_DIR}/aquamacs/build/sign-release "${ARM_DIR}" "${OUTPUT_BUNDLE}"
