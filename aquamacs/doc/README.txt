Instructions on creating Aquamacs HTML Help and the Aquamacs manual

1. Working installations of LaTeX and latex2html are required, for
example using the Texlive distribution. The most recent version of latex2html can be downloaded at www.latex2html.org. (latex2html requires the netpbm library. This can be easily installed along with its dependencies via DarwinPorts (sudo port install netpbm)).

2. Special fonts are needed:

curl -O http://tug.org/fonts/getnonfreefonts/install-getnonfreefonts
sudo texlua install-getnonfreefonts
sudo getnonfreefonts-sys --all


3. 

cd doc/latex
make



