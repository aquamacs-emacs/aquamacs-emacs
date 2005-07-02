Instructions on creating Aquamacs HTML User Help

1. First, go to www.latex2html.org and download/install the most recent version of latex2html.

2. Replace the installed config file l2hconf.pm with the modified version from Aquamacs CVS.  It should go in /usr/local/lib/latex2html.

3. Replace the icon files with the modified versions. These should be installed in /usr/local/share/lib/latex2html/icons. 

4. To generate the HTML files for Aquamacs, cd to where the LaTeX file is, then open "aquamacs.tex" in a text editor. You will need to modify the settings for a table that will not render correctly in the HTML version. Find the table with the caption "Standard locations for Aquamacs user customizations" (a simple search should take you there). Comment out the line at the top of the table that says "\begin{sidewaystable}[t]" by putting a % sign in front of the line.  Uncomment out the next line that says "\begin{table}[t]" by removing the %. Go down to the bottom of the table's text and comment out the  "\end{sidewaystable}" line and uncomment the "\end{table}." Then, save the file. This change is necessary because the table is very wide and must be displayed sideways in the printed LaTeX version; the table must be displayed in the standard format in the HTML output. 

5. After saving the file, run this command:

latex2html -local_icons -html_version=4.1 aquamacs.tex

This will create a subdirectory called "aquamacs" in the LaTeX source directory.

5. Replace "aquamacs.css" in the HTML directory with the "aquamacs.css" file from the parent LaTeX directory. The "aquamacs.css" file in the LaTeX directory is a modified version that will display the user help in accordance with Apple interface guidelines. 

6. Add these tags to "index.html" in the HTML directory:

<META NAME="AppleTitle" content="Aquamacs Help"> 
<META NAME="AppleFont" content="Lucida Grande,Helvetica">

These tags will allow the Apple Help indexing system to search the HTML files. 

7. Run Apple's Help Indexer (from Developer Tools) on the HTML directory. Create indexes for 10.3 and 10.4.
