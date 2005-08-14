#!/bin/sh
#
# You need to be an Aquamacs developer at sourceforge in order to
# run this script. 

SOURCEFORGEUSERNAME=davidswelt

scp ${SOURCEFORGEUSERNAME}@shell.sourceforge.net:/home/groups/a/aq/aquamacs/logs/version-queries.log .

shopt -s xpg_echo

echo "user\tcalls\tvers\ttime\n" >stats.txt

#perl -ne 'use Time::ParseDate; /^(.*)\t.*sess=(\-?[0-9]*)\&.*seq=([0-9]*)\&.*ver=([^\&\n]*)/ig; $ep = parsedate($1); print "$2\t$3\t$4\t$ep\n";' <version-queries.log >>stats.txt
 

cat version-queries.log | ./calc-stats.perl

# stats can now be processed with R 

R --no-restore --no-save < stats.R
