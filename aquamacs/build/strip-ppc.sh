#!/bin/sh

open ~/Aquamacs/AquamacsInstall.dmg
cd /Volumes/Aquamacs\ Emacs
cd Aquamacs.app/Contents/MacOS

for f in * bin/* libexec/*; do
 lipo -remove ppc7400 $f -output $f
done
