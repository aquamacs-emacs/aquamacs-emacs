#!/bin/bash
#
# Aquamacs Emacs -- nightly build script
#
# This script and build process are intended for internal use only, to
# compile and provide an unsigned binary distribution each night.
#
# NIGHTLY_DIR - specifies the directory to be used for the nightly build.
# BRANCH - controls the git branch used for the build. Defaults to aquamacs3.
# NOGIT=1 - suppresses fetching of the main branch from github. This
#    is particularly useful when debugging this script.
# DOC_FAILED_EXIT=0 to continue building if the documentation build
#    fails. Normally it should not be overridden, but it is sometimes
#    necessary.
#
# The file env.sh in ${NIGHTLY_DIR} can be used to override any of
# these variables.
#
# If these variables are changed, the documentation in
# aquamacs/build/BUILD-AQUAMACS.txt should be updated as well.


NIGHTLY_DIR=~/Nightly
BRANCH=aquamacs3
NOGIT=
DOC_FAILED_EXIT=1

# Exit if any command fails
set -e

# Set server-specific (non-public) env variables
. ${NIGHTLY_DIR}/env.sh

# Set internal variables
SRCDIR=${NIGHTLY_DIR}/aquamacs-emacs.git
LOGDIR=${NIGHTLY_DIR}/logs
DOC_LOG=${LOGDIR}/aquamacs-doc-$(date '+%Y-%m-%d-%H%M').log
BUILD_LOG=${LOGDIR}/aquamacs-build-$(date '+%Y-%m-%d-%H%M').log

[ -d ${LOGDIR} ] || mkdir -p ${LOGDIR}
# This exec command forces both stdin and stderr to a log file, so we
# don't have to carefully log the output of every command.

exec &> >(tee ${BUILD_LOG})

cd ${SRCDIR}
echo "Begin building Aquamacs at" $(date)

if [ "${NOGIT}x" = x ]; then
    echo "Updating working directory from Git repository."
    git fetch -f origin
    git branch -D new-$BRANCH >>/dev/null
    git checkout -f --track -b new-$BRANCH origin/$BRANCH  \
        && git branch -D $BRANCH  \
        && git branch -m new-$BRANCH $BRANCH
else
    echo "NOGIT is set; skipping git fetch"
fi

echo "Latest change:"
git log --oneline -1
echo ""

# make doc often creates stuff, which subsequent git updates don't overwrite
git clean -f aquamacs/doc/

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
DATE=`date +"%Y-%b-%d-%a-%H%M"`
BLD=`pwd`/builds/Aquamacs-${DATE}.tar.bz2
[ -d builds ] || mkdir builds

aquamacs/build/build.sh -release
echo "Building Aquamacs application...done"

echo "Copying Homebrew libraries to app bundle."
aquamacs/build/build-homebrew-libraries.sh -bundle ${APP}


echo "Packaging Aquamacs"
(cd `dirname ${APP}` && tar cjf ${BLD} Aquamacs.app)

# copy-build-to-server will be called by postprocessing script
# running this whether it's successful or not.

echo "Postprocessing..."
. ../post.sh ${BLD} ${DATE} # do server-specific post-processing (webserver copy)
