#!/usr/bin/perl

# Note: Developer Tools needed.
# install the Intel Aquamacs build in
# /Applications/Aquamacs Emacs.app
# then run this. 

$PROJECT_DIR = '/Users/dr/Projects/Aquamacs';
$REPO_DIR = '/Users/dr/aquamacs-emacs';

$BINARY = '/Applications/Aquamacs.app';


$DMG = "$PROJECT_DIR/AquamacsInstall.dmg";
# The source DMG is prepared, with a background, and folders .fseventsd (with empty no_log file),
# and .Trash

$RELEASE_NOTES = "$REPO_DIR/aquamacs/doc/latex/changes.pdf";

$VERS=&sys("perl -ne 'print \$1 if (/defvar *aquamacs-version *\"(.*?)\"/);print \$1 if (/defvar *aquamacs-minor-version *\"(.*?)\"/);' < \"$BINARY/Contents/Resources/lisp/aquamacs/site-start.el\"");

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
 
&sys("cd \"$BINARY\"; \"$REPO_DIR/aquamacs/build/sign\"");

&sys("cp  -pR \"$BINARY\" \"$VOL/\"");
  } else 
  {
    warn "couldn't find proper volume. abort. vol=$VOL";
    exit;
  }
 

# do this in the target
&sys("find \"$VOL/Aquamacs.app/Contents/Resources/\" -name \"*~\" -exec rm -f {} \\;");
&sys("find \"$VOL/Aquamacs.app/\" -name \"#*#\" -exec rm -f {} \\;");

@V = glob("\"$VOL/Release\ Notes*.pdf\"");
$RN= $V[0];

# &sys("DeRez -only icns \"$VOL/Manual.pdf\" >/tmp/icon1.r");
# &sys("DeRez -only icns \"$RN\" >/tmp/icon2.r");

$VERS =~ s/preview/pr/i;
$RN2 = "$VOL/Release Notes $VERS.pdf";
$MN2 = "$VOL/Manual.pdf";

&sys("mv \"$RN\" \"$RN2\"");

&sys("cp -p \"$REPO_DIR/aquamacs/doc/latex/changes.pdf\" \"$RN2\"");
# the following to keep the icon's position
&sys("cp -p \"$REPO_DIR/aquamacs/doc/latex/aquamacs.pdf\" \"$MN2\"");

# &sys("Rez /tmp/icon2.r -o \"$RN2\" ;  SetFile -a C \"$RN2.pdf\"");
# &sys("Rez /tmp/icon1.r -o \"$MN2\" ;  SetFile -a C \"$MN2\"");

# set custom icons
&sys("SetFile -a C \"$RN2.pdf\"");
&sys("SetFile -a C \"$MN2\"");


# hide extension in Finder
&sys("SetFile -a E \"$RN2\"");
&sys("SetFile -a E \"$MN2\"");

&sys("rm -rf \"$VOL/.Trashes/*\"");



# doesnt work - cant mount anything afterwards
## hdiutil unmount "$VOL"  

&sys("rm $target");

#&sys("cp AquamacsInstall.dmg AquamacsInstall.R.dmg");
# &sys("hdiutil resize -sectors `hdiutil resize AquaMacsInstall.R.dmg | awk \{print\ \$1\}`"); 

#&sys("hdiutil resize AquaMacsInstall.R.dmg -sectors 407568");

&sys("hdiutil detach \"$VOL\"");
#&sys("hdiutil convert  \"$DMG\" -format UDBZ -o $target");
&sys("rm $target 2>/dev/null");
&sys("open \"$DMG\"");

print("hdiutil convert  \"$DMG\" -format UDBZ -o $target\n");

# format UDBZ would be smaller (bzip2), but incompatible with OS X 10.3


sub sys($)
  {
    print $_[0]."\n";
    return qx($_[0]);
  }


