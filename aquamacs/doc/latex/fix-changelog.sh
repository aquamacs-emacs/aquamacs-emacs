#!/bin/bash

# This script extracts the changelog from the HTML version of the manual

orig=`pwd`

newdir="$1" || newdir=aquamacs/

cd "${newdir}"
echo `pwd`
echo $(/usr/bin/grep -m1 -l changelog-top *.html)
echo `/usr/bin/grep -m1 -l changelog-top *.html`
chgfile=$(/usr/bin/grep -m1 -l changelog-top *.html)
echo "CHGFILE=$chgfile"

[ -z $CHGFILE ] && echo "Could not find CHGfile." >&2 ; exit 1

cp ${chgfile} ${chgfile}.bak 

(cat ${chgfile}.bak | perl -e 'my $x=join("",<STDIN>); $x=~s!(<H2>.*?)(<a name="changelog-top"></a>)!\2\1!s; print($x);' > ${chgfile} ) 

rm "${chgfile}.bak" 2>/dev/null

echo "Changelog: writing to" ${orig}/changelog.html
(cat ${chgfile} | perl -e 'my $x=join("",<STDIN>); $x=~s|^.*(<a name="changelog-top"></a>)(.*?)<!--Navigation Panel-->.*</HTML>|\2|s; print($x);' > ${orig}/changelog.html ) 

cd ${orig}
