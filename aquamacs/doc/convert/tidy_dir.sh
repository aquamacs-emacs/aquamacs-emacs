#!/usr/bin/env bash
FILES=./*.html

for f in $(find . -iname '*.html')
do
  echo "Processing $f..."
  `tidy -q -asxhtml -indent -clean -modify $f`
done
