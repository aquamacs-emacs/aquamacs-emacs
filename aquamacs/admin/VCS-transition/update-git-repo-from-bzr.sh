#!/bin/sh


cd ~/Repositories

cd notengoamigos.bzr/trunk
bzr pull
cd ../..


mv emacs.trunk.bzr.marks emacs.trunk.bzr.marks.bak
mv emacs.trunk.git.marks emacs.trunk.git.marks.bak
cd emacs.git
bzr fast-export -v --import-marks=../emacs.trunk.bzr.marks.bak --export-marks=../emacs.trunk.bzr.marks  --git-branch=master --checkpoint=120000 ../notengoamigos.bzr/trunk >changes
git fast-import --import-marks=../emacs.trunk.git.marks.bak --export-marks=../emacs.trunk.git.marks --force <changes

cd ..
D=`date "+%Y%m%d%H%M%S"`
tar czf bak/$D.marks.tgz emacs.trunk.*.marks



