#!/bin/sh
#
#       Sample shell script to fetch your project HTML
#
/usr/bin/wget -q -O /home/groups/a/aq/aquamacs/projhtml.tmp 'http://sourceforge.net/export/projnews.php?group_id=138078&limit=3&flat=1&show_summaries=1'  > /dev/null
/bin/mv -f /home/groups/a/aq/aquamacs/projhtml.tmp /home/groups/a/aq/aquamacs/htdocs/news.cache
