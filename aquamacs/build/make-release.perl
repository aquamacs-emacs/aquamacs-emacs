#!/usr/bin/perl

# Note: Developer Tools needed.
# install the Intel and PPC Aquamacs builds in
# /Applications/Aquamacs Emacs.app and
# /Applications/Aquamacs Emacs PPC.app
# then run this. 

$PROJECT_DIR = '/Users/dr/Projects/Aquamacs';
$REPO_DIR = '/Users/dr/ae.git';

$BINARY = '/Applications/Aquamacs.app';


$DMG = "$PROJECT_DIR/AquamacsInstall.dmg";
$RELEASE_NOTES = "$REPO_DIR/aquamacs/doc/latex/changes.pdf";

$VERS=&sys("perl -ne 'print \$1 if (/defvar *aquamacs-version *\"(.*?)\"/);print \$1 if (/defvar *aquamacs-minor-version *\"(.*?)\"/);' < \"$BINARY/Contents/Resources/site-lisp/site-start.el\"");

$target = "\"/Users/dr/Desktop/Aquamacs-Emacs-$VERS.dmg\"";

print $VERS,"\n";



&sys("hdiutil detach /Volumes/Aqua*");

&sys("hdiutil attach \"$DMG\"");

@V = glob("/Volumes/Aqua*");
$VOL= $V[0];
unless ($VOL)
  { 
    warn "couldnt find proper volume. abort. vol=$VOL";
    exit;
  
  }

print "Copying to $VOL";

if ($VOL =~ /\/Volumes\/Aquam/i)
  {

&sys("rm -rf \"$VOL/Aquamacs.app\"");
 
&sys("cp  -pR \"$BINARY\" \"$VOL/\"");
  } else 
  {
    warn "couldnt find proper volume. abort. vol=$VOL";
    exit;
  }
 

# do this in the target
&sys("find \"$VOL/Aquamacs.app/Contents/Resources/\" -name \"*~\" -exec rm -f {}\\;");
&sys("find \"$VOL/Aquamacs.app/\" -name \"#*#\" -exec rm -f {}\\;");

&sys("find \"$VOL/Aquamacs.app/\" -name \"#*#\" -exec rm -f {}\\;");

&sys("DeRez -only icns \"$VOL/Aquamacs Manual.pdf\" >/tmp/icon1");
&sys("DeRez -only icns \"$VOL/Release Notes\"*.pdf >/tmp/icon2");



&sys("cp -p \"$REPO_DIR/aquamacs/doc/latex/changes.pdf\" \"$VOL/Release Notes $VERS.pdf\"");
# the following to keep the icon's position

&sys("cp -p \"$REPO_DIR/aquamacs/doc/latex/aquamacs.pdf\" \"$VOL/Aquamacs Manual.pdf\"");

&sys("Rez /tmp/icon1 -o \"$VOL/Release Notes $VERS.pdf\" ;  SetFile -a C \"$VOL/Release Notes $VERS.pdf\"");
&sys("Rez /tmp/icon2 -o \"$VOL/Aquamacs Manual.pdf\" ;  SetFile -a C \"$VOL/Aquamacs Manual.pdf\"");


&sys("rm -rf \"$VOL/.Trashes/*\"");



# doesnt work - cant mount anything afterwards
## hdiutil unmount "$VOL"  

&sys("rm $target");

#&sys("cp AquamacsInstall.dmg AquamacsInstall.R.dmg");
# &sys("hdiutil resize -sectors `hdiutil resize AquaMacsInstall.R.dmg | awk \{print\ \$1\}`"); 

#&sys("hdiutil resize AquaMacsInstall.R.dmg -sectors 407568");

&sys("hdiutil detach \"$VOL\"");
&sys("hdiutil convert  \"$DMG\" -format UDZO -imagekey  zlib-level=9 -o $target");

# format UDBZ would be smaller (bzip2), but incompatible with OS X 10.3


sub sys($)
  {
    print $_[0]."\n";
    return qx($_[0]);
  }


