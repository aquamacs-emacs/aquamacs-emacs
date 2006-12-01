# $Id: Makefile,v 1.1 2006/12/01 23:31:41 davidswelt Exp $
# Makefile for the GNU Emacs lisp library, Elib
# Copyright (C) 1991-1995 Free Software Foundation
#
# This file is part of the GNU Emacs lisp library, Elib.
#
# GNU Elib is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Elib is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to
# the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
#

# ================================================================
# Change the following to reflect the situation at your site:

prefix = /usr/local
datadir = $(prefix)/share
locallisppath = $(datadir)/emacs/site-lisp
# This will fail if locallisppath is anything but a single directory.
# That is all right, since that is the default behaviour of Emacs; those
# that know how to change it, should know how to change this file.  And
# if this is accepted into GNU Emacs, the files should end up inside
# the normal lisp directory.
ELIBDIR = $(locallisppath)/elib
infodir = $(prefix)/info

EMACS=/Applications/Aquamacs Emacs.app/Contents/MacOS/Aquamacs Emacs

MAKEINFO = makeinfo
TEXI2DVI = texi2dvi
SHELL = /bin/sh
INSTALL_DATA = cp

# ================================================================
# You shouldn't change anything below this line.
#

VERSION = 1.0

ELFILES = stack-f.el stack-m.el queue-f.el queue-m.el \
elib-node.el bintree.el avltree.el string.el read.el cookie.el dll.el \
dll-debug.el

ELCFILES = stack-f.elc stack-m.elc queue-f.elc queue-m.elc\
elib-node.elc bintree.elc avltree.elc string.elc read.elc cookie.elc dll.elc \
dll-debug.elc

all: elcfiles elib-startup.el info

elib-startup.el: startup-template.el Makefile
	sed s,ELIB_PATH,\"$(ELIBDIR)\", < startup-template.el > elib-startup.el

install: all installdirs install-info
	$(INSTALL_DATA) $(ELFILES) $(ELIBDIR)
	$(INSTALL_DATA) $(ELCFILES) $(ELIBDIR)
	@echo Please merge elib-startup.el into $(locallisppath)/default.el

installdirs:
	-mkdir $(ELIBDIR)

install-info: elib.info
	$(INSTALL_DATA) elib.info* $(infodir)
	if $(SHELL) -c 'install-info --version' \
		>/dev/null 2>&1; then \
		install-info --infodir=$(infodir) elib.info; \
	else true; fi

clean:
	$(RM) *~ elib-startup.el core $(ELCFILES)
	$(RM) -r $(DISTDIR) $(DISTDIR).tar.gz $(DISTDIR).tar.gz.uu


info: elib.info

elib.info: elib.texi
	$(MAKEINFO) elib.texi

dvi: elib.dvi

elib.dvi: elib.texi
	$(TEXI2DVI) $(srcdir)/elib.texi

.PHONY: elcfiles
elcfiles:
	"$(EMACS)" -batch -l elib-compile-all.el -f compile-elib

TAGS tags:
	etags $(ELFILES)

# Below this point is rules for release management.

OTHERS = README INSTALL COPYING ChangeLog elib.texi gpl.texi \
	startup-template.el elib-compile-all.el Makefile \
	.cvsignore RELEASING TODO NEWS elib-test.el


DISTDIR = elib-$(VERSION)

dist: disttree
	rm -f $(DISTDIR).tar.gz $(DISTDIR).tar.gz.uu
	tar cvf $(DISTDIR).tar $(DISTDIR)
	gzip $(DISTDIR).tar

disttree:
	rm -rf $(DISTDIR) || exit 0
	mkdir $(DISTDIR)
	cp $(ELFILES) $(DISTDIR)
	cp $(OTHERS) $(DISTDIR)

# eof
