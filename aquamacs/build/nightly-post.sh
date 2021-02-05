#!/bin/sh
#
# Post the build to S3
# Usage:
#    post.sh <full-path-to-tar.bz2-file>

BUCKET=aquamacs
BUILD=$1
DEST=aquamacs-nightly.tar.bz2

aws s3 cp $1 s3://aquamacs/${DEST} --acl public-read
