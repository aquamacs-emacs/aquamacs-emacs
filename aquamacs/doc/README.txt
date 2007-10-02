Instructions on creating Aquamacs HTML User Help

1. Working installations of LaTeX and latex2html are required, for
example using the Texlive distribution. The most recent version of latex2html can be downloaded at www.latex2html.org. (latex2html requires the netpbm library. This can be easily installed along with its dependencies via DarwinPorts (sudo port install netpbm)).

2. cd to the build directory of the Aquamacs CVS tree and run
"make-documentation." This will build the PDF and HTML versions of the
user docs. 

3. (Important) Run Apple's Help Indexer (from Developer Tools) on the AquamacsHelp directory that contains the HTML-based help. Create indexes for 10.3 and 10.4. 

When building Aquamacs (copy-to-app step), the HTML help (for Apple
Help) will be automatically copied to the right place. When making a distribution package (copy-to-dmg, not public yet), the PDF manual is copied to the DMG.


