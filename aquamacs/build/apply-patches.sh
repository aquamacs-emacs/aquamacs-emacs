# apply all patches in one go
# start this from the Aquamacs build directory
# or set environment variables AQUAMACS_ROOT and EMACS_ROOT

# the directory structure must be
# aquamacs/ Aquamacs CVS checkout
# emacs / GNU Emacs CVS checkout


if [ ! ${AQUAMACS_ROOT} ]
then  
    AQUAMACS_ROOT=`pwd`/../aquamacs
fi

if [ ! ${EMACS_ROOT} ]
then
    EMACS_ROOT=${AQUAMACS_ROOT}/../emacs
fi


cd ${EMACS_ROOT}
echo "soft-wrap (experimental)"
patch -p0 <${AQUAMACS_ROOT}/patches/soft-wrap.patch 
echo "mac-modifier-keys"
patch -p0 <${AQUAMACS_ROOT}/patches/mac-modifier-keys.patch 
echo "transparency2"
patch -p0 <${AQUAMACS_ROOT}/patches/transparency2.patch 
echo "calm-startup"
patch -p0 <${AQUAMACS_ROOT}/patches/calm-startup.patch 
echo "available-screen"
patch -p0 <${AQUAMACS_ROOT}/patches/available-screen.patch
echo "toolbar-button"
patch -p0 <${AQUAMACS_ROOT}/patches/toolbar-button.patch
echo "org-gnu-Aquamacs"
patch -p0 <${AQUAMACS_ROOT}/patches/org-gnu-Aquamacs.patch
echo "puresize"
patch -p0 <${AQUAMACS_ROOT}/patches/puresize.patch
echo "eval-depth"
patch -p0 <${AQUAMACS_ROOT}/patches/eval-depth.patch


