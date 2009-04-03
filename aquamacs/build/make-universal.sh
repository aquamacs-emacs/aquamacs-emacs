#!/bin/bash

# usage: <path-to-destination-app> <path-to-app>

# merges two .app emacsen (for PPC and for intel architectures) into one universal .app


DEST_ARCH=$1
OTHER_ARCH=$2

cd "${DEST_ARCH}"
F=`ls Contents/MacOS/bin/* Contents/MacOS/libexec/*` 
for file in $F; do
  echo $file
  lipo -create "$file" "${OTHER_ARCH}/$file" -output "$file.univ" 
  mv $file.univ "$file" 2>/dev/null
done
echo "Aquamacs Emacs"
lipo -create "Contents/MacOS/Aquamacs Emacs" "${OTHER_ARCH}/Contents/MacOS/Aquamacs Emacs" -output "Contents/MacOS/Aquamacs Emacs.univ"
mv "Contents/MacOS/Aquamacs Emacs.univ"  "Contents/MacOS/Aquamacs Emacs"
