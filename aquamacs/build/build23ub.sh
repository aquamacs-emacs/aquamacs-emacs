#!/bin/sh

# Build Aquamacs
# (universal binary)
# for internal use only

# This builds a UB of Aquamacs
# We're building all architectures separately (via incremental build where possible) in order to be
# able to specify individual optimizations.  Some optimizations don't work for PPC, and cross-compiling
# with dumping requires gcc-4.0 (for the PPC build).
# With earlier versions that specified two -arch arguments in CFLAGS and performed surgery on the
# `temacs' right before dumping, we were unable to do -fast or even -O2.

# The list of optimizations for PPC has been hand-crafted from what is available on gcc-4.0
# and what is acceptable to the cross-compile+dump process.


MACOSX_DEPLOYMENT_TARGET="10.5"
export MACOSX_DEPLOYMENT_TARGET

# not good with 4.0 at least: -fstrict-aliasing -fstrict-overflow    -ftree-vrp 
# not good w/ Emacs: -freorder-functions
#
XC_PPC_OPT='-fdefer-pop -fguess-branch-probability -fcprop-registers   -fif-conversion -fif-conversion2 -ftree-ccp -ftree-dce -ftree-dominator-opts -ftree-dse -ftree-ter    -ftree-lrs -ftree-sra -ftree-copyrename  -ftree-fre -ftree-ch  -funit-at-a-time -fmerge-constants -fthread-jumps -fcrossjumping -foptimize-sibling-calls -fregmove  -frerun-cse-after-loop -fcaller-saves -fpeephole2 -fexpensive-optimizations  -ftree-pre  -fschedule-insns -fcse-skip-blocks -fgcse  -fgcse-lm   -fcse-follow-jumps    -fschedule-insns2 -fsched-interblock  -fsched-spec -fdelete-null-pointer-checks -freorder-blocks  -falign-functions  -falign-jumps -falign-loops  -falign-labels -finline-functions -funswitch-loops'

# for PPC Cross-Compile we must use gcc-4.0:  
# with 4.2, something goes wrong during dumping with pcc ("Wrong type argument: integerp, 3.0")
# gcc-4.5 isn't accepted by the configure script
 
# on Intel (at least on X86_64), -fast produces fast code, but
# an Aquamacs that crashes when started even with -Q.
# for now, we're leaving out -fomit-frame-pointer, -fstrict-aliasing, -momit-leaf-frame-pointer

architectures="i386 ppc"

i386_CC='gcc'
i386_CFLAGS="-O3 -fno-tree-pre -falign-loops -arch i386"
i386_LDFLAGS="-O3 -fno-tree-pre -falign-loops -arch i386"

ppc_CC='gcc-4.0'
ppc_CFLAGS="$XC_PPC_OPT -arch ppc"
ppc_LDFLAGS="$XC_PPC_OPT -arch ppc"

bin_dest="nextstep/Aquamacs.app/Contents/MacOS"


# The per-architecture build process is relatively transparent:
# we run `configure' with the right parameters, clean out lib-src (workaround).
# binaries in lib-src and src are copied after the build to make way for the next architecture
# Once all architectures have been built, we use `lipo' to join them, taking care to
# 


make clean
find . -type f -perm +ugo+x -name '*.arch.*' -delete
rm src/DOC-*

for arch in $architectures; do

echo "Current architectures:"
ls -la src/emacs-*

echo "Building for architecture: $arch"
VNAME=${arch}_CFLAGS
eval A_CFLAGS=\$$VNAME

VNAME=${arch}_LDFLAGS
eval A_LDFLAGS=\$$VNAME

VNAME=${arch}_CC
eval A_CC=\$$VNAME

echo PATH=/bin:/sbin:/usr/bin ./configure --with-ns --without-x CC="${A_CC}" CFLAGS="${A_CFLAGS}" LDFLAGS="${A_LDFLAGS}"
PATH=/bin:/sbin:/usr/bin ./configure --with-ns --without-x CC="${A_CC}" CFLAGS="${A_CFLAGS}" LDFLAGS="${A_LDFLAGS}"

cd lib-src; make clean; cd -  # lib-src doesn't rebuild itself when architecture changes (src does)
make # this should do bootstrap if necessary

# identify all executables
# and move them
for f in `find lib-src -type f -perm +ugo+x`; do
if [[ $f != *.arch.* ]] ; then   # ignore existing architecture-specific files
    if [[ `lipo -info $f 2>/dev/null` == *on-fat*${arch}* ]] ; then
	echo $f
	cp -p $f $f.arch.${arch}   # copy, not move, so we leave something for the next step to find
    fi
fi
done
mv src/emacs src/emacs.arch.${arch}
rm src/emacs-2*  # remove file with version number (next build must use same version number: DOC files, etc!)

done  # with each architecture

# identify all executables
# and join the together

for f in `find lib-src -type f -perm +ugo+x`; do
  if [[ $f != *.arch.* ]] ; then   # ignore existing architecture-specific files
	  # join
      num_a=$(ls $f.arch.* 2>/dev/null | wc -l)
      if [ $num_a != "0" ]; then
	  echo lipo -create $f.arch.* -o $f
	  lipo -create $f.arch.* -o $f
	   # prevent make from thinking this is a new file:
	  # choose the latest build for the time stamp
	  touch -r `ls -to $f.arch.* | head -n1` $f  
      fi
	  # get rid of architecture-specific files
	  #rm $f.arch.*
   fi
done
lipo -create src/emacs.arch.* -o src/emacs
touch src/emacs # ensure latest time stamp (otherwise, install(blessmail) will force re-dumping)
#the following are not needed (the src/make all will copy it over for us)
#cp -p src/emacs `ls src/emacs-2* | head -n1`
#cp -p src/emacs nextstep/Aquamacs.app/Contents/MacOS/Aquamacs

make install

echo "Finished building for the following architectures:"
lipo -info nextstep/Aquamacs.app/Contents/MacOS/Aquamacs
