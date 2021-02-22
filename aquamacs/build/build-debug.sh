#!/bin/bash
#
# Build Aquamacs with debugging enabled
# Should be run as
#     aquamacs/build/build-debug.sh
# from the top level of the Aquamacs source tree.

# Options for debugging: one for ./configure, one for CFLAGS

NS_TRACE_FLAGS="-DNSTRACE_ENABLED=1"
NS_TRACE_FLAGS+=" -DNSTRACE_ALL_GROUPS=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_EVENTS=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_UPDATES=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_FRINGE=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_COLOR=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_GLYPH=1"
NS_TRACE_FLAGS+=" -DNSTRACE_GROUP_FOCUS=1"

# Override NS_TRACE_FLAGS for now
NS_TRACE_FLAGS=

# Turn on assertion checking: --enable-checking

# It would be nice to use this, but it looks like there are a lot of
# problems with assertions that are not shedding light on the main
# bugs now, and they also seem to be in areas that have substantial
# changes in Emacs 27/28.

#export DEBUG_CONFIG_OPTS="--enable-checking"
export DEBUG_CONFIG_OPTS=""
export DEBUG_CFLAGS="-O0 -g3 ${NS_TRACE_FLAGS} -fno-omit-frame-pointer"


export MIN_VERSION=
./build-aquamacs
