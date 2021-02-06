#!/bin/bash
#
# Build Aquamacs with debugging enabled
# Should be run as
#     aquamacs/build/build-debug.sh
# from the top level of the Aquamacs source tree.

# Options for debugging: one for ./configure, one for CFLAGS
DEBUG_CONFIG_OPTS="--enable-checking='yes,glyphs' --enable-check-lisp-object-type"
DEBUG_CFLAGS='-O0 -g3'

./build-aquamacs
