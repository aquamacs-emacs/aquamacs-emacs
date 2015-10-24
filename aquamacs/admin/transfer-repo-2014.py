#!/usr/bin/env python
from __future__ import print_function

# This script was designed to transfer and rebase commits on the
# Aquamacs branches from the pre-2015 repository onto the 2015 Emacs
# repository.
# It fails to do so successfully.
# One reason may be an incorrect selection of revisions ("ancestry path"),
# but another one may be that by checking out individual commits,
# we ignore the conflict resolutions developed at merge time during
# the later merge commits.

# A transfer using "reposurgeon" was attempted before crafting this,
# but it ultimately failed for other reasons.


import sys
import os

resume = None
#resume = "4e5ee3d600ee9d19062fc588d268237377b54c72"


revidlinks = {'5027ce036f0aac25a2424db0a952a64974331bc9':'b4fcdf02b899f6f9003dcb80543de7e62eebfeca'}


newrepo = "/Users/dr/emacs"
srcdir = "/Users/dr/ae24.git"
tmpdir = "/tmp/patches"
treedir = tmpdir+"/emacs"

def run(shcmd, path=treedir, error="stop"):
    print (path, shcmd)
    #    var = raw_input("Yes (press enter)?")
    var = ""
    if var=="" or var=="\n":
        os.chdir(path)
        r= os.system(shcmd)
        if r != 0 and error=="stop":
            print ("Warning - command exited with %s"%r)
            var = raw_input("Next (press enter)?")
        if r != 0:
            return r
    else:
        print ("prevented.")
    return False

import re
def grepcount(file, regexp):
    c = 0
    for line in open(file, 'r'):
        if re.search(regexp, line):
            c += 1
    return c
def grep(file, regexp):
    c = []
    for line in open(file, 'r'):
        if re.search(regexp, line):
            c += [line]
    return c

def failure_backup (c):
    global srcdir
    global tmpdir

    run("git am --abort", error=False)

    run("rm -rf *", srcdir, error=False)
    run("rm -f .*", srcdir, error=False)
    if run("git checkout -f "+c, srcdir):
        raise "failed to check out"

    run("git reset")
    run("cp -rp * %s/"%treedir, srcdir)  # copy everything over
    run("git add --all")
    run("git log -1 --pretty=%B >"+tmpdir+"/message", srcdir)
    run("git commit -F %s/message"%tmpdir)



# the previous Aquamacs repository contained two copies of Emacs (from when Bazaar mirrors changed)
# we exclude commits from both of them

if not resume:
    os.system("mkdir %s"%tmpdir)

    run("git rev-list  --ancestry-path e3571d3...aquamacs3 ^emacs24 ^origin/emacs23 ^origin/emacs ^sv/emacs-23 ^sv/emacs-24 > %s/aq-commits.txt"%tmpdir, srcdir)
    run("git log --ancestry-path e3571d3...aquamacs3 ^emacs24 ^origin/emacs23 ^origin/emacs ^sv/emacs-23 ^sv/emacs-24 > %s/aq-log-commits.txt"%tmpdir, srcdir)

    run("rm -rf %s"%treedir, tmpdir)
    run("cp -rp %s %s"%(newrepo,treedir), tmpdir)

    # the following prevent us from making a root commit
    run("rm -f .git/hooks/pre-commit .git/hooks/commit-msg", treedir)


    run("git checkout -f master", treedir)  #
    run("rm -rf *", treedir)  # clean it out
    run("git branch -D aquamacs3", treedir)  # delete branch
    run("git checkout --orphan aquamacs3", treedir)  # create new empty branch
    run("git rm -rf .", treedir)
    run("git commit -m 'New branch for Aquamacs'", treedir, error=False) # maybe "nothing to commit"

# before there is a commit on this branch, it doesn't have a name (ref) it seems
#run("git checkout -f aquamacs3", treedir)  # create new empty branch

# first aq commit is e3571d3

commits = [x.strip('\n') for x in open("%s/aq-commits.txt"%tmpdir).readlines()]
commits += ['e3571d3655d1bc019757d29d26522e67a515364e']
previous = None
for c in reversed(commits):
    if resume==c:
        resume = None
    if not resume:

        if previous:
            # range = "%s..%s"%(previous,c)
            range = "-1 "+c
        else:
            range = "-1 "+c
            previous = "0000000"

        patch = "%s/%s.patch"%(tmpdir,c)

        regular = True

        if c in revidlinks:
            regular = False

        if regular:
            run("git format-patch --stdout %s > %s"%(range, patch), srcdir)
            # if os.path.getsize(patch)>2500000:
            np = grepcount(patch, r'^Subject:')
            if np>1 and grepcount(patch, r'Reitter')<np*0.2:
                regular = False

        # this is essentially only for the first merge 5027ce03
        if not regular:
            with open(patch, 'w') as f:
                date = "".join(grep(patch, r'^Date:'))
                print("From: David Reitter <david.reitter@gmail.com>\nDate: "+date+"\nSubject: Merge\n\n---\n", file=f)

            run("git diff --full-index %s %s >> %s"%(previous, c, patch), srcdir)  # concatenates

        if run("git am --ignore-whitespace --whitespace=nowarn <%s"%patch, treedir, error=False):
            # failed
            failure_backup(c)


        if not regular and c in revidlinks:
            # mark the merge with a merge commit (if parents known)(
            run("git merge -s ours --no-commit "+revidlinks[c])
            run("git commit -m 'Merge\nPrevious commit contains merge'")


        # var = raw_input("Next (press enter)?")
    previous = c

# whitespace needs to be ignored.  If we fix it, later patches may fail.


# grep -ce "^Subject: " /tmp/patches/5027ce036f0aac25a2424db0a952a64974331bc9.patch

# if more than 3 commits within one patch file, it is probably a bigger merge
#     (perhaps it's not  a merge of my own stuff, e.g. aquamacs2 -> aquamacs3)


# last ressort
# check if patch fails
# if it does, clean out directory (except for .git),
#     go to source directory, clean it out, check out the revision in question
#     git archive or git-checkout-index and copy everyting over
#     somehow add current state (must pick up deleted files... )
#     git add --all  (should pick up deleted files)
#     and commit
