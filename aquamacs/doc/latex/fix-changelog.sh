#!/bin/bash

# This script extracts the changelog from the HTML version of the manual

orig=`pwd`

newdir="$1" || newdir=aquamacs/

cd "${newdir}"
echo `pwd` >&2
echo $(/usr/bin/grep -m1 -l changelog-top *.html) >&2
echo `/usr/bin/grep -m1 -l changelog-top *.html` >&2
chgfile="$(/usr/bin/grep -m1 -l changelog-top *.html)"
echo "chgfile=$chgfile" >&2

[ -z "$chgfile" ] && echo "Could not find chgfile." >&2

cp ${chgfile} ${chgfile}.bak 

(cat ${chgfile}.bak | perl -e 'my $x=join("",<STDIN>); $x=~s!(<H2>.*?)(<a name="changelog-top"></a>)!\2\1!s; print($x);' > ${chgfile} ) 

rm "${chgfile}.bak" 2>/dev/null

echo "Changelog: writing to" ${orig}/changelog.html
(cat ${chgfile} | perl -e 'my $x=join("",<STDIN>); $x=~s|^.*(<a name="changelog-top"></a>)(.*?)<!--Navigation Panel-->.*</HTML>|\2|s; print($x);' > ${orig}/changelog.html ) 

cd ${orig}
