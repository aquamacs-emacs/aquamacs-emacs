;;; carbon-font.el -- fontsets for Carbon Emacs -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004-2005 by T. Hiromatsu <matsuan@users.sourceforge.jp>
;; Version 1_2_2

;;; Commentary:

;; This package defines fixed-width multilingual fontsets for Carbon Emacs
;; on Mac OS X. Comments, questions and feedback will be sent to an english
;; list <http://lists.sourceforge.jp/mailman/listinfo/macemacsjp-english>
;; of MacEmacs JP project <http://macemacsjp.sourceforge.jp/en/>.
;;----------------------------------------------------------------------
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License can be gotten from
;; the Free Software Foundation, Inc.,
;;     59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;     http://www.gnu.org/licenses/gpl.html
;;
;;----------------------------------------------------------------------
;;      $BK\%W%m%0%i%`$O%U%j!<!&%=%U%H%&%'%"$G$9!#(B
;;      $B$"$J$?$O!"(BFree Software Foundation$B$,8xI=$7$?(BGNU $B0lHL8xM-;HMQ5vBz$N(B
;;      $B!V%P!<%8%g%s#2!W0?$$$O$=$l0J9_$N3F%P!<%8%g%s$NCf$+$i$$$:$l$+$rA*Br$7!"(B
;;      $B$=$N%P!<%8%g%s$,Dj$a$k>r9`$K=>$C$FK\%W%m%0%i%`$r(B
;;      $B:FHRI[$^$?$OJQ99$9$k$3$H$,$G$-$^$9!#(B
;;
;;      $BK\%W%m%0%i%`$OM-MQ$H$O;W$$$^$9$,!"HRI[$K$"$?$C$F$O!"(B
;;      $B;T>l@-5Z$SFCDjL\E*E,9g@-$K$D$$$F$N0EL[$NJ]>Z$r4^$a$F!"(B
;;      $B$$$+$J$kJ]>Z$b9T$J$$$^$;$s!#(B
;;      $B>\:Y$K$D$$$F$O(BGNU $B0lHL8xM-;HMQ5vBz=q$r$*FI$_$/$@$5$$!#(B
;;
;;      GNU$B0lHL8xM-;HMQ5vBz$O!"!!(B
;;      Free Software Foundation,
;;         59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
;;         http://www.gnu.org/licenses/gpl.html
;;      $B$+$iF~<j2DG=$G$9!#(B
;;
;;----------------------------------------------------------------------
;; carbon-font.el 2005-04-17$BHG(B
;;
;; 1. Introduction
;; 1.1. $B$3$N%U%!%$%k$NCf?H(B
;;	$B$3$N%U%!%$%k$O!"(Bcarbon emacs on Mac OSX $B$G!"(B2$B%P%$%HJ8;z$H!"(Bascii$B$r(B
;;	1:2$B$NI}$G(B($B=j0bEyI}(B)$B$G!"I=<($9$k$?$a$N(Bfontset$BDj5A$NNc$r<($7$F$$$^$9!#(B
;;
;;	$BDj5A$7$F$$$k$N$O!"2<5-$N(B4$B<o$NJ8;z%;%C%H$G$9!#(B
;;	    hiraginomaru**    Monaco(Ayuthaya) + $B%R%i%.%N4]%4(B
;;	    hiraginokaku**    Monaco(Ayuthaya) + $B%R%i%.%N3Q%4(B
;;	    osaka**           Monaco(Ayuthaya) + osaka
;;	    hiraginomin**     Courier  + $B%R%i%.%NL@D+(B
;;
;;	**$B$NItJ,$O%5%$%:$G!"(Bascii$BJ8;z$N(Bpoint$B?t$rI=$7$^$9!#$3$N%U%!%$%k$G$O!"(B
;;	    07,08,09,10,12,14,16,18,20,24
;;	$B$rDj5A$7$F$$$^$9!#99$K!"(Bosaka$B$N$_(B15point$B$rDj5A$7$F$$$^$9!#$D$^$j!"(B
;;	$B$3$N%U%!%$%k$G$O!"(B41$B<o$N!"(Bfontset $B$rDj5A$7$F$$$k$3$H$K$J$j$^$9!#(B
;;
;;	$B<B:]$N(B fontset$BL>$O!"(Bhiraginomaru14 $BEy$N$h$&$K@.$j$^$9!#(B
;; 
;;      $B$3$N%P!<%8%g%s$+$i!"=>Mh$N(B carbon-font $B$G$O=PMh$J$+$C$?!"F|K\8l%U%)(B
;;      $B%s%H$N(Bface-font$BBP1~$7$F$$$^$9!#(B
;;      $BJ?$?$/1>$($P!"(Binfo$BEy$G!"Bg$-$J;z$K%j%9%1!<%k$7$?$j!"%\!<%k%I$d!"%9(B
;;      $B%i%s%H%U%'!<%9$K$9$k0Y$NJ*$G$9!#(B
;;
;;      $B8=:_!"BP1~$7$F$$$k$N$O!"(Bfontset-osaka** $B$N$_$G$9!#(B
;;          08,09,10,12,15,20  $B$O!"0l1~$^$H$b$KI=<($G$-$F$$$k$h$&$G$9!#(B
;;      $BB>$K$*5$IU$-$K$J$C$?$3$H$,M-$j$^$7$?$i!"%a!<%j%s%0%j%9%H$K$4Js9p4j(B
;;      $B$($k$H9,$$$G$9!#(B
;;
;; 1.2. $BF0:n4D6-(B
;;	carbon emacs $B$O!"(B2005-11-30 $B0J9_$N(BCVS$B$+$iF~<j$7$?J*$r8f;H$$$/$@$5$$!#(B
;;	$B$=$l0JA0$NJ*$O!"(B.emacs$BFI$_9~$_$N;~$K%(%i!<$K$J$k2DG=@-$,$"$j$^$9!#(B
;;
;;      OSX $B$N%$%s%9%H!<%k$N;~$K!"4J0W%$%s%9%H!<%k$r$5$l$?>l9g!"(BAyuthaya$B$,(B
;;      $B%$%s%9%H!<%k$5$l$F$$$^$;$s!#$3$N>l9g!"%5%$%:$O!"(B09,10,12,14$B$N$_$H$J$j(B
;;      $B$^$9!#(B
;;
;;	07,08,16,18,20,24 $B$r$*;H$$$K$J$j$?$$>l9g$O!"(BInstaller CD$B$NCf$+$i!"(B
;;	    AdditionalFonts.pkg
;;	$B$rC5$7=P$7$F!"%$%s%9%H!<%k$7$F$/$@$5$$!#(B
;;
;;	$B<o!9$JCN7C$r<x$1$F$/$@$5$C$?!"(Bmac-emacsen ML $B$d(B 2ch mac de emacs$B2q5D(B
;;	$B<<$NJ}!9$K46<U$7$^$9!#(B
;;
;; 2. Usage($B;H$$J}(B)
;; 2.0. $BCm0UE@(B
;;     $B0JA0!"(B
;;	   face-font-rescale-alist
;;     $B$N@_Dj$,I,MW$G$7$?$,!"8=:_$OITMW$G$9!#<+F0$GJQ99$5$l$^$9!#(B
;;     $B$h$C$F!"$J$K$b$7$J$/$F$b!"(Bbold$B$H!"(Bmedium$B$NI}$OF1$8$K$J$j$^$9!#(B
;;     
;;     $B$b$7!"(Bsilk$B$r$*;H$$$N>l9g$O!"<+F0JQ99$O<YKb$G$9$+$i!"(B
;;	    (setq carbon-font-auto-rescale-enable nil)
;;     $B$7$F$/$@$5$$!#(B
;;
;; 2.1. $BFI$_9~$_J}(B
;;     $B$3$N%U%!%$%k$r!"(B~/$B$KCV$$$F!"(B.emacs$B$K(B   
;;          (if (eq window-system 'mac) (load "~/carbon-font"))
;;     $B$H$7$F$=$N8e$G!";H$&(Bfontset $B$N0Y$K(B
;;          (set-default-font "fontset-hiraginomaru16")
;;     $BEy$H!"@_Dj$7$F$/$@$5$$!#(B
;;
;;     $BKt!"(Bload-path $B$NDL$C$?$H$3$m$KCV$$$F!"(B
;;          (if (eq window-system 'mac) (require 'carbon-font))
;;     $B$H$7$F$b!"%m!<%I$5$l$^$9!#(B
;;
;;     $B:G=i$O!"(Bfontset$B$N;XDj$r$7$J$$$G!"(Bshift + click $B$G=P$F$/$k!"(Bfontset
;;     menu $B$+$i!"$*9%$_$NJ*$rC5$9J}$,NI$$$+$b$7$l$^$;$s!#(B
;;
;;     $BCm(B: carbon emacs $B$G!"(Biso-2022-7bit $B8GM-$JJ8;z$r(B kill & yank $B$9$k>l9g(B
;;     $BLdBj$,$"$k$h$&$J$N$G!"(B.emacs$B$K$3$N%U%!%$%k$+$i(B kill & yank $B$9$k$h$j!"(B
;;     $B$3$N%U%!%$%k$r$=$N$^$^$*;H$$$K$J$k$h$&$*4+$a$7$^$9!#(B
;;
;;     $B$b$7!"(Bkill & yank $B$9$k>l9g$O!"0l;~E*$K(B
;;     (set-clipboard-coding-system            'iso-2022-7bit)
;;     $B$7$F$/$@$5$$!#(B
;;
;; 2.2. ascii bold $B$rEyI}$GI=<($9$k(B
;;
;; 2.2.1. face-font-rescale-alist $B$r@_Dj$9$k(B(default)
;;      Bold$B$bF1$8$K$J$k$h$&!"(Bfont $B$NBg$-$5$r!"(Bface-font-rescale-alist $B$G!"(B
;;      $BD4@0$7$F$$$^$9!#(B
;;
;;     $BESCf$G!"(Bfontset-menu$B$+$i(Bfontset$B$rA*$s$@>l9g$b!"<+F0$GJQ99$5$l$^$9!#(B
;;     $B7gE@(B: Bold$B$N9b$5$,Dc$/$J$k(B
;;     $BH~E@(B: elisp$B$@$1$G<B8=$G$-$k(B
;;
;; 2.2.2. Silk Sytem Preferences $B$r;H$&(B
;;     Silk $B%7%9%F%`4D6-@_Dj$N(B
;;         "Global/Application Settings"$B$N(B
;;             "Use Quartz Text Metrics" $B$K(Bcheck$B$rF~$l$k!#(B
;;         $B$3$N;~!"(BAdd Application$B$G!"(BEmacs$B$rDI2C$7$F!"(BEmacs$B$N$_$K$3$NB0@-(B
;;         $B$rM?$($?J}$,NI$$$H;W$o$l$^$9!#(B
;;
;;     $BKt!"(B (setq carbon-font-auto-rescale-enable nil) $B$,I,MW$G$9!#(B
;;     
;;     $B7gE@(B: $BM>7W$J%7%9%F%`4D6-@_Dj$rF~$l$kI,MW$,M-$k!#(B
;;           $B7y$$$J?M$K$H$C$F$OCWL?E*(B
;;     $BH~E@(B: Bold Font$B$b!"(Bnormal Font $B$HF1$89b$5$GI=<($G$-$k!#(B
;;     
;;     silk $B$O2<5-%5%$%H$GF~<j$G$-$^$9!#(B
;;     http://www.unsanity.com/
;;
;; 2.3. Alias $B$,3]$i$J$$>l9g(B
;;     $B>.$5$$%U%)%s%H$N>l9g!"(Baliasing$B$,>e<j$/3]$i$J$$>l9g$,M-$j$^$9!"$=$N;~(B
;;     $B$O!"0J2<$NMM$K$7$F$_$F$/$@$5$$!#(B
;;     (setq mac-allow-anti-aliasing t)
;;
;; 2.4. $BCf9q8l!"4Z9q8l$r$-$A$s$HI=<($5$;$k(B
;;     (utf-translate-cjk-mode t)
;;     $B$r!"%;%C%H$9$k$H!"$3$N%U%!%$%k$N!"Cf9q8l!"4Z9q8l$N%U%)%s%HL>$b$-$A$s(B
;;     $B$HI=<($5$l$^$9!#(B($BH&$G$9(B)
;;
;; 3. Changelog
;; 1_2_2   2005-05-21 Tiger$BBP:v(B
;;         monaco $B$,I=<($G$-$J$$%5%$%:$N(Balternative$B$H$7$F!"(Bayuthaya $B8GDj$G$O$J$/(B
;;         $B?7$?$K!"JQ?t(B carbon-font-monaco-alternative $B$r@_$1$F!"%G%U%)%k%H$K(B
;;         "lucida sans typewriter" $B$r@_Dj!#(B
;; 1_2_1 2005-04-17 $B0J2<$rDI2C(B
;;         mac-roman-lower, mac-roman-upper,
;;         mule-unicode-0100-24ff, mule-unicode-2500-33ff,
;;         mule-unicode-e000-ffff, iso10646-1
;;
;; 1_2_0   2005-03-27 $BAjED$5$s$NJQ99$r<h$j9~$_(B
;;         fontset $B@_Dj$N4X?t2=(B
;;         iso8859-15 $B$rDI2C(B
;;         
;; 1_1_1   2005-02-27
;;         centraleurope$B!"(Bcyrillic $B$N(BFont$B$rD4@0(B
;; 1_1_0   2005-01-30   
;;         centraleurope$B!"(Bcyrillic $B$K(B $BBP1~(B
;;         osaka$B$N0lIt$K!"(Bfont-face $BBP1~(B
;;
;; 1_0_2   2005-01-16
;;         courier new $B$NB8:_$r<+F0H=JL$9$k$h$&$KJQ99(B
;; 1_0_1   2004-12-12
;;         ayuthaya font $B$N(Bencode$BL>$,!"(Bcyrillic$B$K@.$C$?$N$KH<$$!"1S$_(B
;;         $B9~$a$J$/$J$C$?$N$G!"(BFIX$B!#(B
;; 1_0_0   2000-11-14 $BHG$r!"(B1_0_0 $B$H$9$k!#(B
;;
;; 3.2$BHG(B   2004-11-14
;;         $B%^%$%J!<%P%0%U%#%C%/%9(B & $B4X?tL>@0M}(B
;;          Doc. $B@0M}!"G[I[>r7oJQ99(B
;; 3.1$BHG(B   2004-10-11
;;	   ayuthaya $B$N<+F0H=DjJ}K!JQ99(B
;;         hiraginomin07,09,10,16$B%Q%i%a!<%?JQ99(B
;;	   hiraginomin07,16$B$K!"(Bcourier new $B;HMQ(B
;; 3  $BHG(B   2004-10-10
;;	   face-font-rescale-alist $B$r<+F0@_Dj2=(B
;;
;; 2.3$BHG(B   2004-10-09
;;	   carbon-font-sub$B<h$j9~$_(B
;; 2.2$BHG(B   2004-05-30
;;	   $B@bL@DI2C(B
;;         bug$B=$@5(B
;; 2.1$BHG(B   2004-05-12
;;         bug$B=$@5(B
;;         Osaka$BDI2C(B
;;
;; 2  $BHG(B   2004-05-04
;;         Apple $BM3Mh$N(BFont$B$@$1$K$9$k$?$a!"(BAndale mono $B$r!"(BAuthaya $B$KJQ99(B
;;         $B%R%i%.%N3Q%4!"%R%i%.%NL@D+$rDI2C(B
;;         07,08 point $B$rDI2C(B
;; 
;; $B=iHG(B    2004-03-16
;;                                                  T.Hiromatsu
;;                                                  matsuan@users.sourceforge.jp

;;
;; fontset section
;;

(defun carbon-font-concat-with-comma (&rest args)
  (mapconcat 'identity (remq nil args) ","))

(defun carbon-font-spec-japanese-generic (name size pt)
  (carbon-font-concat-with-comma
   (format "japanese-jisx0208:-apple-%s-medium-r-normal--%d-%d-75-75-m-%d-jisx0208.1983-sjis" 
           name size pt pt)
   (format "japanese-jisx0201:-apple-%s-medium-r-normal--%d-%d-75-75-m-%d-jisx0201.1976-0" 
           name size pt pt)))

(defun carbon-font-spec-chinese-std (size pt)
  (carbon-font-concat-with-comma
   (format "chinese-gb2312:-apple-$A;*ND:ZLe(B-medium-r-normal--%d-%d-75-75-m-%d-gb2312.1980-0" 
           size pt pt)
   (format "chinese-big5-1:-apple-$(0\cE*(B pro-medium-r-normal--%d-%d-75-75-m-%d-big5-0" 
           size pt pt)))

(defun carbon-font-spec-korean-std (size pt)
  (format "korean-ksc5601:-apple-applegothic-medium-r-normal--%d-%d-75-75-m-%d-ksc5601.1989-0" 
          size pt pt))

(defvar carbon-font-monaco-alternative "lucida sans typewriter"
  "Monaco alternative font name")

(defvar carbon-font-monaco-alternative-exist (x-list-fonts (concat "*-" carbon-font-monaco-alternative "-*"))
  "Variable if monaco alternative font is installed or not.")

(defvar carbon-font-courier_new-exist (x-list-fonts "*-courier new-*")
  "Variable if courier new font is installed or not.")

(defvar carbon-font-monaco_cy-exist (x-list-fonts "*-monaco cy-*")
  "Variable if monaco cy font is installed or not.")

(defvar carbon-font-monaco_ce-exist (x-list-fonts "*-monaco ce-*")
  "Variable if monaco ce font is installed or not.")

(defun carbon-font-spec-family-monaco (size ascsize alternative)
  (carbon-font-concat-with-comma
   (format "ascii:-apple-%s-medium-r-normal--%d-*-*-*-m-*-mac-*"
;;    (format "ascii:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
;   (format "latin-iso8859-1:-apple-monaco-medium-r-normal--%d-*-75-75-m-*-mac-roman" size)
   (format "latin-iso8859-1:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "greek-iso8859-7:-apple-symbol-medium-r-normal--%d-*-75-75-m-*-mac-symbol" size)
;;    (format "latin-iso8859-7:-apple-monaco-medium-r-normal--%d-*-75-75-m-*-mac-roman" size)
;;    (format "latin-iso8859-7:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
;;            (if alternative carbon-font-monaco-alternative "monaco")
;;            ascsize)
;   (format "latin-iso8859-9:-apple-monaco-medium-r-normal--%d-*-75-75-m-*-mac-roman" size)
   (format "latin-iso8859-9:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "latin-iso8859-15:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "mac-roman-lower:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "mac-roman-upper:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "mule-unicode-0100-24ff:-apple-symbol-medium-r-normal--%d-*-75-75-m-*-mac-symbol" size)
;;    (format "mule-unicode-0100-24ff:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
;;            (if alternative carbon-font-monaco-alternative "monaco")
;;            ascsize)
   (format "mule-unicode-2500-33ff:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "mule-unicode-e000-ffff:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "iso10646-1:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-*"
           (if alternative carbon-font-monaco-alternative "monaco")
           ascsize)
   (format "latin-iso8859-2:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-centraleurroman"
           (if carbon-font-monaco_ce-exist "monaco ce" "*")
           size)
   (format "cyrillic-iso8859-5:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-cyrillic"
	   (if carbon-font-monaco_cy-exist "monaco cy" "*")
	   size)
   ))

(defun carbon-font-spec-family-courier (size ascsize cnew)
  (carbon-font-concat-with-comma
   (format "ascii:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           ascsize)
   (format "latin-iso8859-1:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "latin-iso8859-15:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "mac-roman-lower:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "mac-roman-upper:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "mule-unicode-0100-24ff:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "mule-unicode-2500-33ff:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "mule-unicode-e000-ffff:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "iso10646-1:-apple-%s-*-*-normal--%d-*-75-75-m-*-mac-roman"
           (if cnew "courier new" "courier")
           size)
   (format "latin-iso8859-2:-apple-courier ce-medium-r-normal--%d-*-75-75-m-*-mac-centraleurroman" size)
   (format "cyrillic-iso8850-5:-apple-%s-medium-r-normal--%d-*-75-75-m-*-mac-cyrillic"
           (if (<= size 12) "lucida grande cy" "*")
           size)
   ))

;;
;; hiraginomaru** = $B%R%i%.%N4]%4(B + monaco(alternative) 
;;

(defun carbon-font-create-fontset-hiraginomaru (size cjksize cjkpt ascsize alternative)
  (if (or (not alternative) carbon-font-monaco-alternative-exist)
      (create-fontset-from-fontset-spec
       (carbon-font-concat-with-comma 
        (format "-*-fixed-medium-r-normal-*-%d-*-*-*-*-*-fontset-hiraginomaru%02d" size size)
        (carbon-font-spec-japanese-generic "$B%R%i%.%N4]%4(B pro w4" cjksize cjkpt)
        (carbon-font-spec-chinese-std cjksize cjkpt)
        (carbon-font-spec-korean-std cjksize cjkpt)
        (carbon-font-spec-family-monaco size ascsize alternative)
        ))))

(carbon-font-create-fontset-hiraginomaru 24 28 288 22 nil)
(carbon-font-create-fontset-hiraginomaru 20 24 240 19 nil)
(carbon-font-create-fontset-hiraginomaru 18 22 216 18 nil)
(carbon-font-create-fontset-hiraginomaru 16 20 200 16 nil)
(carbon-font-create-fontset-hiraginomaru 14 16 160 14 nil)
(carbon-font-create-fontset-hiraginomaru 12 14 140 12 nil)
(carbon-font-create-fontset-hiraginomaru 10 12 120 10 nil)
(carbon-font-create-fontset-hiraginomaru  9 12 120  9 nil)
(carbon-font-create-fontset-hiraginomaru  8 10 100  8 nil)
(carbon-font-create-fontset-hiraginomaru  7  8  80  7 nil)

;;
;; hiraginokaku** = $B%R%i%.%N3Q%4(B + monaco(alternative) 
;;

(defun carbon-font-create-fontset-hiraginokaku (size cjksize cjkpt ascsize alternative)
  (if (or (not alternative) carbon-font-monaco-alternative-exist)
      (create-fontset-from-fontset-spec
       (carbon-font-concat-with-comma 
        (format "-*-fixed-medium-r-normal-*-%d-*-*-*-*-*-fontset-hiraginokaku%02d" size size)
        (carbon-font-spec-japanese-generic "$B%R%i%.%N3Q%4(B pro w3" cjksize cjkpt)
        (carbon-font-spec-chinese-std cjksize cjkpt)
        (carbon-font-spec-korean-std cjksize cjkpt)
        (carbon-font-spec-family-monaco size ascsize alternative)
        ))))

(carbon-font-create-fontset-hiraginokaku 24 28 288 22 nil)
(carbon-font-create-fontset-hiraginokaku 20 24 240 19 nil)
(carbon-font-create-fontset-hiraginokaku 18 22 216 18 nil)
(carbon-font-create-fontset-hiraginokaku 16 20 200 16 nil)
(carbon-font-create-fontset-hiraginokaku 14 16 160 14 nil)
(carbon-font-create-fontset-hiraginokaku 12 14 140 12 nil)
(carbon-font-create-fontset-hiraginokaku 10 12 120 10 nil)
(carbon-font-create-fontset-hiraginokaku  9 12 120  9 nil)
(carbon-font-create-fontset-hiraginokaku  8 10 100  8 nil)
(carbon-font-create-fontset-hiraginokaku  7  8  80  7 nil)

;;
;; hiraginomin** = $B%R%i%.%NL@D+(B + courier 
;;

(defun carbon-font-create-fontset-hiraginomin (size cjksize cjkpt ascsize cnew)
  (if (or (not cnew) carbon-font-courier_new-exist)
      (create-fontset-from-fontset-spec
       (carbon-font-concat-with-comma 
        (format "-*-fixed-medium-r-normal-*-%d-*-*-*-*-*-fontset-hiraginomin%02d" size size)
        (carbon-font-spec-japanese-generic "$B%R%i%.%NL@D+(B pro w3" cjksize cjkpt)
        (carbon-font-spec-chinese-std cjksize cjkpt)
        (carbon-font-spec-korean-std cjksize cjkpt)
        (carbon-font-spec-family-courier size ascsize cnew)
        ))))

(carbon-font-create-fontset-hiraginomin 24 28 288 24 nil)
(carbon-font-create-fontset-hiraginomin 20 22 220 19 nil)
(carbon-font-create-fontset-hiraginomin 18 22 216 18 nil)
(carbon-font-create-fontset-hiraginomin 16 20 200 16 nil)
;; (carbon-font-create-fontset-hiraginomin 16 20 200 16   t)
(carbon-font-create-fontset-hiraginomin 14 16 160 14 nil)
(carbon-font-create-fontset-hiraginomin 12 14 140 12 nil)
(carbon-font-create-fontset-hiraginomin 10 12 120 10 nil)
(carbon-font-create-fontset-hiraginomin  9 10 100  9 nil)
(carbon-font-create-fontset-hiraginomin  8 10 100  8 nil)
;; (carbon-font-create-fontset-hiraginomin  7  8  80  7 t)
(carbon-font-create-fontset-hiraginomin  7  8  80  7 nil)

;;
;; osaka** = osaka + monaco(alternative) 
;;

(defun carbon-font-create-fontset-osaka (size cjksize cjkpt alternative japanese)
  (if (or (not alternative) carbon-font-monaco-alternative-exist)
      (create-fontset-from-fontset-spec
       (carbon-font-concat-with-comma 
        (format "-apple-osaka-medium-r-normal-*-%d-*-*-*-*-*-fontset-osaka%02d" size size)
        (and japanese (carbon-font-spec-japanese-generic "osaka" cjksize cjkpt))
        (carbon-font-spec-chinese-std cjksize cjkpt)
        (carbon-font-spec-korean-std cjksize cjkpt)
        (carbon-font-spec-family-monaco size size alternative)
        ))))

(carbon-font-create-fontset-osaka 24 28 288 nil   t)
(carbon-font-create-fontset-osaka 20 24 240 nil nil)
(carbon-font-create-fontset-osaka 18 22 216 nil nil)
(carbon-font-create-fontset-osaka 16 20 200 nil nil)
(carbon-font-create-fontset-osaka 15 18 180 nil nil)
(carbon-font-create-fontset-osaka 14 16 160 nil   t)
(carbon-font-create-fontset-osaka 12 14 140 nil nil)
(carbon-font-create-fontset-osaka 10 12 120 nil nil)
(carbon-font-create-fontset-osaka  9 12 120 nil nil)
(carbon-font-create-fontset-osaka  8 10 100 nil nil)
(carbon-font-create-fontset-osaka  7  8  80 nil nil)

;;
;;  $BJQ?t(B section
;;

;;  $B;HMQ$9$k%U%)%s%H%;%C%H$rJQ$($?8e!"<+F0$G!"%\!<%k%I$r%j%9%1!<%k$5$;$k!#(B
;;  $B$b$7!"(BSilk$B$r;H$&>l9g$O!"(Bnil$B$K%;%C%H(B

(defvar carbon-font-auto-rescale-enable t
  "Enable auto set face-font-rescale-alist after changing font.")

;;  $B%\!<%k%I$r%j%9%1!<%k$9$k0Y$N%U%!%/%?!<$NDj5A(B

(defvar carbon-font-scale-alist-without-silk
  '(("ayuthaya-bold" .   (
                          ("07". 0.8)
                          ("08". 0.95)
                          ("09". 0.85)
                          ("10". 0.9)
                          ("12". 0.9)
                          ("14". 0.9)
                          ("16". 0.95)
                          ("15". 0.95)
                          ("18". 0.9)
                          ("20". 0.95)
                          ("24". 1.0)))
    ("monaco-bold" . (
                          ("07". 0.8)
                          ("08". 0.95)
                          ("09". 0.90)
                          ("10". 0.8)
                          ("12". 0.9)
                          ("14". 0.9)
                          ("16". 0.95)
                          ("15". 0.95)
                          ("18". 0.9)
                          ("20". 0.95)
                          ("24". 0.92)))
;;     ("ayuthaya-medium" . (("08". 1.1)
;;                           ("09". 1.1)
;;                           ("20". 1.1)
;;                           ("24". 1.1)))
    ("courier-bold" . (   ("07". 1.0)
                          ("09". 0.9)
                          ("10". 0.9)))
    ("courier-medium" . ( ("07". 1.0)
                          ("20". 1.1)))
    ("osaka-medium-r" .  (
                          ("07". 1.15)
                          ("08". 1.25)
                          ("09". 1.35)
                          ("10". 1.2)
                          ("12". 1.2)
                          ("15". 1.2)
                          ("16". 1.25)
                          ("18". 1.25)
                          ("20". 1.2)
                          ))
    ("osaka-medium-i" .  (
                          ("07". 1.15)
                          ("08". 1.25)
                          ("09". 1.35)
                          ("10". 1.2)
                          ("12". 1.2)
                          ("15". 1.2)
                          ("16". 1.25)
                          ("18". 1.25)
                          ("20". 1.2)
                          ))
    ("osaka-bold" .      (
                          ("08". 1.2)
                          ("09". 1.3)
                          ("10". 1.10)
                          ("12". 1.15)
                          ("15". 1.15)
                          ("16". 1.20)
                          ("18". 1.20)
                          ("20". 1.15)
                          ))
    )
  "ReScale factor alist for each fonts and size without silk.")

(defvar carbon-font-scale-alist-with-silk
  '(("ayuthaya-medium" . (("20". 1.1)
                          ("24". 1.1)))
    ("courier-medium" . ( ("20". 1.1)))
    ("osaka-medium" . (
;;                          ("07". 1.1)
                          ("08". 1.2)
                          ("09". 1.2)
                          ("10". 1.2)
                          ("12". 1.2)
;;                          ("14". 1.3)
                          ("15". 1.2)
;;                          ("16". 1.3)
;;                          ("18". 1.2)
                          ("20". 1.2)
;;                          ("24". 1.3)
                          ))
    ("osaka-bold" . (
;;                          ("07". 1.2)
                          ("08". 1.3)
                          ("09". 1.3)
                          ("10". 1.3)
                          ("12". 1.3)
;;                          ("14". 1.4)
                          ("15". 1.2)
;;                          ("16". 1.4)
;;                          ("18". 1.3)
                          ("20". 1.3)
;;                          ("24". 1.4)
                          ))
    )
  "ReScale factor alist for each fonts and size with silk.")

;;  $B4X?tDj5A(B
;;
;;  $B%+%l%s%H%U%l!<%`$G;H$o$l$F$$$k%U%)%s%H$N!"%j%9%1!<%k%U%!%/%?!<$r!"(B
;;  carbon-font-scale-alist $B$+$i!"<hF@$9$k!#(B

(defun carbon-font-get-scale (font-name)
  "current frame $B$G!"(Bfont-name $B$N!"(Brescale parameter $B$rJV$9(B"
  (let
      ((x
        (cdr (assoc (substring (cdr (assoc 'font (frame-parameters))) -2 nil)
                    (assoc font-name
                           (if carbon-font-auto-rescale-enable
                               carbon-font-scale-alist-without-silk
                             carbon-font-scale-alist-with-silk))
                    ))
        ))
    (if x x 1.0)
    ))

;;  $B<hF@$7$?%j%9%1!<%k%U%!%/%?!<$G!"(Bface-font-rescale-alist$B$r=q$-49$($k!#(B

(defun carbon-font-append-factor-recursively (scale-list)
  "Append rescale list to 'face-font-rescale-alist"
  (setq face-font-rescale-alist
        (append
         (list (cons (concat ".*" (car (car scale-list)) ".*")
                     (carbon-font-get-scale (car (car scale-list)))
                     ))
         (delete (assoc (concat ".*" (car (car scale-list)) ".*")
                        face-font-rescale-alist) face-font-rescale-alist)
         ))
  (if (cdr scale-list)
      (carbon-font-append-factor-recursively (cdr scale-list))))

;; (defun carbon-font-set-rescale-alist ()
;;   "Append rescale list to 'face-font-rescale-alist" 
;;   (dolist (elt (if carbon-font-auto-rescale-enable
;;                    carbon-font-scale-alist-without-silk
;;                  carbon-font-scale-alist-with-silk))
;;     (setq face-font-rescale-alist
;;           (append
;;            (list (cons (concat ".*" (car elt) ".*")
;;                        (carbon-font-get-scale (car elt))))
;;            (delete (assoc (concat ".*" (car elt) ".*") face-font-rescale-alist)
;;                    face-font-rescale-alist)))
;;     ))

;;  $B%U%)%s%H$,JQ99$5$l$?>l9g$K%U%C%/$r$+$1$F!"(B
;;  carbon-font-append-factor-recursively $B$r5/F0$9$k!#(B

(add-hook 'after-setting-font-hook
          (lambda ()
            (carbon-font-append-factor-recursively
             (if carbon-font-auto-rescale-enable
                 carbon-font-scale-alist-without-silk
               carbon-font-scale-alist-with-silk)
             ))
          )

;; (add-hook 'after-setting-font-hook
;;           'carbon-font-set-rescale-alist)

(provide 'carbon-font)

;;; carbon-font.el ends here