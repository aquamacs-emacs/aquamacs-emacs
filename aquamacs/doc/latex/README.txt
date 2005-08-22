Instructions on creating Aquamacs HTML User Help

1. Working installations of LaTeX and latex2html are required. We base our build on Gerben Wierda's LaTeX distribution, which can be downloaded at /www.rna.nl/tex.html. The most recent version of latex2html can be downloaded at www.latex2html.org.

2. Replace the installed config file l2hconf.pm with the modified version from Aquamacs CVS.  It should go in /usr/local/lib/latex2html.

3. Replace the icon files with the modified versions in the latex/icons directory. These should be installed in /usr/local/share/lib/latex2html/icons. 

4. cd to the build directory of the Aquamacs CVS tree and run "make-documentation." This will build the PDF and HTML versions of the user docs, 

7. (Optional) Run Apple's Help Indexer (from Developer Tools) on the HTML directory. Create indexes for 10.3 and 10.4. 
