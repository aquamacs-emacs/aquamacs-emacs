#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Generic Retry Script
#
# Copyright (C) 2013, Alex Bennée <alex@bennee.com>
# License: GPLv3
#
# I originally wrote this as a wrapper around rsync for dodgy links.
# The basic premise was to keeps retrying the rsync until eventual success.
# Since then I realised it would be far more useful as a generic wrapper
# for any script that may fail or indeed need repeating.
#

from argparse import ArgumentParser
from time import sleep
import sys
import os
import signal
import subprocess
import itertools

#
# Command line options
#
parser=ArgumentParser(description = "Retry wrapper script.")
parser.add_argument('-v', '--verbose', dest="verbose", action='count')
parser.add_argument('-t', '--test', dest="test",
                    action='store_const', const=True,
                    help="Test without retrying")
parser.add_argument('-n', '--limit', dest="limit", type=int,
                    help="Only loop around this many times")
parser.add_argument('--invert',
                    action='store_const', const=True, default=False,
                    help="Invert the exit code test")
parser.add_argument('--delay', type=int, default=5, help="Sleep for N seconds between retries")
parser.add_argument('command', nargs='*', help="The command to run. You can precede with -- to avoid confusion about it's flags")


def become_tty_fg():
    pass
    # os.setpgrp()
    # hdlr = signal.signal(signal.SIGTTOU, signal.SIG_IGN)
    # tty = os.open('/dev/tty', os.O_RDWR)
    # os.tcsetpgrp(tty, os.getpgrp())
    # signal.signal(signal.SIGTTOU, hdlr)


def wait_some(seconds, verbose):
    become_tty_fg()

    try:
        if verbose: print("waiting for %d" % (seconds))
        sleep(seconds)
        return False
    except:
        print ("got exception")
        return True


if __name__ == "__main__":
    args = parser.parse_args()

    if args.verbose: print ("command is %s" % (args.command))
    if args.invert and args.limit is None:
        sys.exit("You must define a limit if you have inverted the return code test")

    for run_count in itertools.count():
        return_code = subprocess.call(args.command, close_fds=True,
                                      preexec_fn=become_tty_fg)
        if args.test == True: break
        if args.limit and run_count >= args.limit: break
        if args.invert and return_code != 0: break
        elif not args.invert and return_code == 0: break

        print ("Run %d times (rc = %d)" % (run_count+1, return_code))

        # now sleep, exit if user kills it
        if wait_some(args.delay, args.verbose): break

    print ("Ran command %d times" % (run_count+1))
