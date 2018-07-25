fname=aquamacs.tex
fpdf=aquamacs.pdf
finalpdf=Aquamacs\ Manual.pdf
cname=changes.tex
pdfdir=$(shell pwd)
srcdir=$(shell pwd)
newdir=$(srcdir)/aquamacs
destdir="$(srcdir)/../Aquamacs Help"
AQUAMACS_DIR=$(srcdir)/../..
AQUAMACS_MAJOR_VERSION=$(shell perl -ne 'print $$1 if (/defvar *aquamacs-version *\"(.*?)\"/);' < $(AQUAMACS_DIR)/src/site-lisp/site-start.el)


phrase=''
newphrase=''

all: pdf html


pdf: aquamacs.tex changelog.tex changes.tex *.pdf
	echo "Making PDF";					   \
	cd $(pdfdir);						   \
	pdflatex -interaction=nonstopmode $(cname)		   ; \
	echo "Running pdflatex on $(cname)..."			   ; \
	echo "Running pdflatex on $(fname)..."			   ; \
	pdflatex -interaction=nonstopmode $(fname)		   ; \
	echo "Running pdflatex again to update references..."	   ; \
	pdflatex -interaction=nonstopmode $(fname)		   ; \
	echo cp $(pdfdir)/$(fpdf) $(pdfdir)/$(finalpdf)	   ; \
	cp $(pdfdir)/$(fpdf) $(pdfdir)/$(finalpdf)	   ; \
	echo "PDF conversion completed, PDF renamed to $(finalpdf)."


html: aquamacs.tex *.pdf
	echo "Running latex2html on $(fname)...";				     \
	rm -r aquamacs; 	\
	../convert/latex2html -init_file l2hconf-local.pm -local_icons -html_version=4.0 $(fname) ; \
	sips -s format png aquamacs-logo.pdf --out $(newdir)/image.png; \
	echo "$(fname) converted to HTML."					     ; \
	cp aquamacs.css $(newdir)/aquamacs.css					     ; \
	sed 's/<META NAME="description" CONTENT="/<META NAME="AppleTitle" content="Aquamacs Help ('"${AQUAMACS_MAJOR_VERSION}"')"><META NAME="AppleFont" content="Lucida Grande,Helvetica"><META NAME="description" CONTENT="Aquamacs Emacs - /' <$(newdir)/index.html >$(newdir)/index2.html	     ; \
	echo "HTML conversion completed."		     ; \
	echo $(phrase) ; \
	mv $(newdir)/index2.html $(newdir)/index.html ; \
	sed 's|<BODY >|<BODY ><a name="AquamacsIndex"></a>|' <$(newdir)/index.html >$(newdir)/index2.html; \
	mv $(newdir)/index2.html $(newdir)/index.html ; \
	tidy -asxhtml -m *.html ; \
	(cd $(srcdir) ; ./fix-changelog.sh $(newdir)) ; \
	echo "Removing extra files." ; \
	cd $(newdir) ; \
	rm $(newdir)/WARNINGS $(newdir)/images.* $(newdir)/*.bak 2>/dev/null ; \
	../convert/tidy_dir.sh ; \
	echo "Making new Aquamacs Help directory, copying new help files."	     ; \
	mv $(newdir)/* $(destdir)/ ; \
	cp $(srcdir)/*.png $(destdir)/			     ; \
	find $(destdir) -name *.helpindex -delete ; \
	echo "Help files copied. Indexing..." ; \
	hiutil -C -a -s en -f $(destdir)"/Aquamacs Help.helpindex" $(destdir)  ; \
	echo "Done."
