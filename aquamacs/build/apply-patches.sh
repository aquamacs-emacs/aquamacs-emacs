# apply all patches in one go
# start this from the Aquamacs build directory
# or set environment variables AQUAMACS_ROOT and EMACS_ROOT

# written by Terry Jones <tcj25@cam.ac.uk>

# Licensed under the GNU Public License, version 2

# This file is part of Aquamacs Emacs.

# the directory structure should be
# aquamacs/ Aquamacs CVS checkout
# emacs/ GNU Emacs CVS checkout

# Add your patch file name here (file names cannot contain spaces).

# toolbar-png:  taken out, because it's slow and we don't need it yet
# DR 12/2006: inserted again, maybe useful for the "sel" images.
#    is it slow with xpm as well?
# taken out (temp):          capslock   causes trouble Qappevent

PATCHES="mac-functions mac-fullscreen emacs-inline transparency
         dtww
         dnd
         fix-configure test ui-strings menu-bar
         toolbar-png 
         custom-verbosity pretty-modeline 
         mouse-echo
         cocoaspell
         flyspell-aspell
         recentf-track-minibuf
         window-kill
         pop-to-buffer display-buffer-reuse
         cus-edit-switch
         ediff-workaround
         faces-speedup
         face-remapping
         calm-startup startup-load-preferences
         available-screen mouse-button launch-browser
         minibuffer-filename
         header-line
         tmm
         sentence-end
         org-gnu-Aquamacs about-aquamacs puresize
         macfns-dialog-menu menu-bar-visible-frame 
         eval-depth
         shut-up python-mode intel"
#  longlines-fillcolumn  out temporarily


# The patches here are specified separately so their order can be defined.

# It might also be useful(?) to touch a .applied file in the patches
# directory to indicate which patches have been applied correctly.
# Then people could re-run apply-patches.sh. You could compare dates
# and apply the patch file if it were newer than the .applied file
# in case you picked up changes to the patch file from CVS.


# If we are passed -t as argument, just echo what would be done.
exec=1
if [ $# -ge 2 ];
then
    if [ "$1" -eq '-t' ];
    then
	exec=0
    fi
fi
 

if [ ! ${AQUAMACS_ROOT} ]
then  
    AQUAMACS_ROOT="`/bin/pwd`/../aquamacs"
fi

if [ ! -d "$AQUAMACS_ROOT" ]
then
    echo "$0: your AQUAMACS_ROOT variable (set to '$AQUAMACS_ROOT') does not contain a directory." >&2
    exit 1
fi

if [ ! ${EMACS_ROOT} ]
then
    EMACS_ROOT="$AQUAMACS_ROOT/../emacs"
fi

if [ ! -d "$EMACS_ROOT" ]
then
    echo "$0: your EMACS_ROOT variable (set to '$EMACS_ROOT') does not contain a directory." >&2
    exit 2
fi

cd ${EMACS_ROOT} || exit 3

for patch in $PATCHES
do
    file="$AQUAMACS_ROOT/patches/$patch.patch"

    if [ ! -f "$file" ]
    then
	echo "$0: WARNING: patch file '$file' does not exist, skipping." >&2
	continue
    fi

    echo "Applying patch: '$patch'"

    if [ $exec -ne 0 ]
    then
	# -l is to ignore whitespace
	patch -lp0 < "$file"
    else
	echo "patch -lp0 < '$file'"
    fi

    if [ $? -ne 0 ]
    then
	# Maybe we should exit here?
	echo "$0: WARNING: patch for file '$file' failed." >&2
    fi
done
