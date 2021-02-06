#!/bin/bash
#
# Aquamacs Emacs -- nightly build script
#
# This script and build process are intended for internal use only, to
# compile and provide an unsigned binary distribution each night.
#
# NIGHTLY_DIR - specifies the directory to be used for the nightly build.
# BRANCH - controls the git branch used for the build. Defaults to aquamacs3.
# REPO - git repository to clone from if no repository is already present
# GIT=no - suppresses fetching of the main branch from github. This
#    is particularly useful when debugging this script.
# POST=no - suppress posting the resulting kit
# DOC_FAILED_EXIT=0 to continue building if the documentation build
#    fails. Normally it should not be overridden, but it is sometimes
#    necessary.
#
# If these variables are changed, the documentation in
# aquamacs/build/BUILD-AQUAMACS.txt should be updated as well.


DEFAULT_REPO="https://github.com/aquamacs-emacs/aquamacs-emacs.git"

NIGHTLY_DIR=${NIGHTLY_DIR:=~/Nightly}
BRANCH=${BRANCH:=aquamacs3}
GIT=${GIT:=yes}
POST=${POST:=yes}
REPO=${REPO:=${DEFAULT_REPO}}
DOC_FAILED_EXIT=${DOC_FAILED_EXIT:1}

# Exit if any command fails
set -e

# Set internal variables
SRCDIR=${NIGHTLY_DIR}/aquamacs-emacs.git
LOGDIR=${NIGHTLY_DIR}/logs
DOC_LOG=${LOGDIR}/aquamacs-doc-$(date '+%Y-%m-%d-%H%M').log
BUILD_LOG=${LOGDIR}/aquamacs-build-$(date '+%Y-%m-%d-%H%M').log

# Make directories if needed
[ -d ${NIGHTLY_DIR} ] || mkdir -p ${NIGHTLY_DIR}
[ -d ${LOGDIR} ] || mkdir -p ${LOGDIR}
[ -d ${SRCDIR} ] || mkdir -p ${SRCDIR}

# This exec command forces both stdin and stderr to a log file, so we
# don't have to carefully log the output of every command.

exec &> >(tee ${BUILD_LOG})

cd ${SRCDIR}

if [ "${GIT}x" = "yesx" ]; then
    if [ ! -e .git ]; then
        echo "Cloning git repository from ${REPO}"
        (cd ..; git clone ${REPO} ${SRCDIR})
    else
        echo "Updating working directory from Git repository."
        git fetch -f origin
    fi
else
    echo "GIT is ${GIT}; skipping git fetch"
fi

# Use || true for 'git branch -D' because the command might fail, which is fine.
git branch -D new-$BRANCH >>/dev/null || true
git checkout -f --track -b new-$BRANCH origin/$BRANCH  \
    && git branch -D $BRANCH  \
    && git branch -m new-$BRANCH $BRANCH

# make doc often creates stuff, which subsequent git updates don't overwrite
git clean -f aquamacs/doc/

echo "Latest change:"
git log --oneline -1
echo ""

echo "Begin building Aquamacs at" $(date)

echo "Building Aquamacs documentation."
echo "Log in ${DOC_LOG}"

# Build the documentation.
# This requires LaTeX and assumes that MacTeX is installed with the
# latex command on the system standard path.

(cd aquamacs/doc/latex; make &> ${DOC_LOG}) \
    || (echo "*** FAILED building documentation"; exit ${DOC_FAILED_EXIT})

echo "Building Aquamacs documentation...done"

echo "Building Aquamacs application..."

APP=`pwd`/nextstep/Aquamacs.app
BUILD_DATE=`date +"%Y-%b-%d-%a-%H%M"`
KIT=`pwd`/builds/Aquamacs-${BUILD_DATE}.tar.bz2

[ -d builds ] || mkdir builds

aquamacs/build/build-aquamacs.sh

echo "Building Aquamacs application...done"

echo "Copying Homebrew libraries to app bundle."
aquamacs/build/build-homebrew-libraries.sh -bundle ${APP}

echo "Signing nightly release"
aquamacs/build/sign-release

echo "Packaging Aquamacs"
(cd `dirname ${APP}` && tar cjf ${KIT} Aquamacs.app)

echo "Postprocessing..."
if [ "${POST}"x = "yesx" ]; then
   aquamacs/build/nightly-post.sh ${BLD} ${DATE}
fi
