#!/bin/sh
#
# use fetch-log manually, which adds to logs/
# supply any argument to avoid re-parsing log files.


shopt -s xpg_echo


if [ $# -eq 0 ]
  then
echo "user\tcalls\tvers\ttime\n" >stats.txt

#perl -ne 'use Time::ParseDate; /^(.*)\t.*sess=(\-?[0-9]*)\&.*seq=([0-9]*)\&.*ver=([^\&\n]*)/ig; $ep = parsedate($1); print "$2\t$3\t$4\t$ep\n";' <version-queries.log >>stats.txt

cat `ls -rt logs/*` | ./calc-stats.perl

fi

# stats can now be processed with R

R --no-restore --no-save < stats.R


# generate a nice html file

OUT=stats/index.html; export OUT

echo >$OUT
echo  "
<html>
<head></head>
<body><h1>Aquamacs User Statistics</h1><p>Note:  data for a few years are missing.  <p><a href=\"http://aquamacs.org\">Aquamacs Website</a><br></br>" >>$OUT

# for i in *.pdf
# do
#  nn=`basename "$i" .pdf`.gif
#  convert $i $nn
#  echo "<img src=\""$nn"\" />  <br>" >>index.html
# done
for i in *.svg
do
 cp $i stats/
 echo "<img width=650 src=\""$i"\" />  <br>" >>$OUT
done

date >>$OUT
echo "<a href=\"http://www.david-reitter.com/\">David Reitter</a></p></body></html>" >>$OUT


# upload straight to Braeburn

rsync -r stats dreitter@cc.ist.psu.edu:Sites/Aquamacs/
