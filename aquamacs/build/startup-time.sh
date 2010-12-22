#!/bin/sh

BIN=nextstep/Aquamacs.app/Contents/MacOS/Aquamacs

echo "failed" >run.log
"$BIN" -q $* -eval '(progn (write-region "OK" nil "run.log") (kill-emacs))' &
sleep 5


if [ `cat run.log` == "failed" ]; 
then 
  echo `date "+%Y-%m-%d"` failed
  kill -term -$$
fi


"$BIN" -q $* -eval '(progn (kill-emacs))'
"$BIN" -q $* -eval '(progn (kill-emacs))'
"$BIN" -q $* -eval '(progn (kill-emacs))'
"$BIN" -q $* -eval '(progn (kill-emacs))'


echo "failed" >run.log
echo "N/A" > time.log
(time "$BIN" -q $* -eval '(progn (write-region "OK" nil "run.log") (kill-emacs))') 2>time.log

echo `date "+%Y-%m-%d"` `git rev-parse --verify --abbrev-ref HEAD` `git rev-parse --verify --short HEAD`  `cat run.log` `grep real time.log | grep -o '0m.*'` `grep user time.log | grep -o '0m.*'`

# in case anything went wrong, kill the process group (this one included)
kill -term -$$
