Instructions on creating Aquamacs HTML Help and the Aquamacs manual

1. Working installations of LaTeX and latex2html are required, for
example using the Texlive distribution. The most recent version of latex2html can be downloaded at www.latex2html.org. (latex2html requires the netpbm library. This can be easily installed along with its dependencies via DarwinPorts (sudo port install netpbm)).

2. Special fonts are needed:

curl -O http://tug.org/fonts/getnonfreefonts/install-getnonfreefonts
sudo texlua install-getnonfreefonts
sudo getnonfreefonts-sys --all

3. Install "tidy"  (from source, https://github.com/w3c/tidy-html5 )
   Modern versions of "hiutil" require XHTML

4. Just run the Makefile:

cd doc/latex
make


(make html creates the help files, make pdf the PDF manuals)


