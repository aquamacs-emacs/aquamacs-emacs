#!/bin/sh
#
# You need to be an Aquamacs developer at sourceforge in order to
# run this script. 

SOURCEFORGEUSERNAME=davidswelt

#ssh ${SOURCEFORGEUSERNAME}@aquamacs.sourceforge.net '/home/groups/a/aq/aquamacs/fetch-log.sh'

#scp -C ${SOURCEFORGEUSERNAME}@aquamacs.sourceforge.net:/home/groups/a/aq/aquamacs/logs/version-queries.log .
scp -C davidswelt_aquamacs@ssh.phx.nearlyfreespeech.net:/home/protected/version-queries.log .

shopt -s xpg_echo

echo "user\tcalls\tvers\ttime\n" >stats.txt

#perl -ne 'use Time::ParseDate; /^(.*)\t.*sess=(\-?[0-9]*)\&.*seq=([0-9]*)\&.*ver=([^\&\n]*)/ig; $ep = parsedate($1); print "$2\t$3\t$4\t$ep\n";' <version-queries.log >>stats.txt
 
cat version-queries.a.log version-queries.log | ./calc-stats.perl

# stats can now be processed with R 

R --no-restore --no-save < stats.R


# generate a nice html file

echo >index.html
echo  " 
<html>
<head></head>
<body><h1>Aquamacs User Statistics</h1><p><a href=\"http://aquamacs.org\">Aquamacs Website</a><br></br>" >>index.html

for i in *.pdf
do
 nn=`basename "$i" .pdf`.gif
 convert $i $nn
 echo "<img src=\""$nn"\" />  <br>" >>index.html
done

date >>index.html
echo "<a href=\"http://www.reitter-it-media.de/\">david.reitter@gmail.com</a></p></body></html>" >>index.html

