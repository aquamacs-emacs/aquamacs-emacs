#!/bin/sh

D=`date +"%Y%m%d%H%M"`
cd logs
ssh davidswelt_aquamacs@ssh.phx.nearlyfreespeech.net 'mv /home/protected/version-queries.log /home/protected/version-queries-s.log'
scp davidswelt_aquamacs@ssh.phx.nearlyfreespeech.net:/home/protected/version-queries-s.log . && \
mv version-queries-s.log version-queries-$D.log
