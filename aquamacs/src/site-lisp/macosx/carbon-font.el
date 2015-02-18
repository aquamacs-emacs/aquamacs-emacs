;;; carbon-font.el -- fontsets for Carbon Emacs -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004-2005 by T. Hiromatsu <matsuan@users.sourceforge.jp>
;; Version 1_5_3
;; 2005-11-17

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
;; carbon-font.el 2005-09-13$BHG(B;;
;;
;;  1. Introduction
;;  1.1. idea
;;      carbon-font provides font-width-compensation for fixed-width
;;      fontset for Emacs on Mac OSX. The reasons are:
;;          Monaco bold has different width from normal font.
;;          CJK font has different width from ascii font. (We want to use
;;          2 times width for CJK).
;;
;;      Defined fontset names are
;;          hiraginomaru (Sans Serif)
;;          hiraginokaku (Sans Serif)
;;          hiraginomin (Serif)
;;          osaka (Sans Serif)
;;
;;      Defined sizes are
;;          point 7,8,9,10,12,14,16,18,20,24
;;
;;      then totally 40 fontsets were defined.
;;
;;  1.2. Emacs version
;;      carbon-font supportw only CVS version of Emacs after June 1st, 2005.
;;
;;  2. Usage
;;  2.1. Installation
;;      Please put two files in the folder on load-path.
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. load package
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;
;;  2.3. set fontset
;;      If font is defined like as 
;;          "fontset-hiraginokaku",
;;      12point of $B%R%i%.%N3Q%4%7%C%/(B(hiraginokaku) is used.
;;
;;      If you want to use other sizes, please use full XLFD name like as 
;;            "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru"
;;
;;  2.3.1. If you are not familiar to emacs lisp, follow this section.
;;
;;      To define default fontset, I suggest you to use
;;      'fixed-width-set-default-fontset as below.
;;         12point
;;             (fixed-width-set-default-fontset "fontset-hiraginokaku")
;;         other sizes
;;             (fixed-width-set-default-fontset
;;                  "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginokaku")
;;
;;      On initial frame, to use different font from default one, please set 
;;          (add-to-list 'initial-frame-alist '(font . "fontset-hiraginomaru"))
;;      or
;;          (add-to-list
;;           'initial-frame-alist
;;           '(font . "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru"))
;;
;;      To change fontset only on current frame, set
;;          (set-frame-font "fontset-hiraginomaru")
;;      or
;;          (set-frame-font
;;           "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru")
;;
;;  2.3.2. If you are familiar to E-lisps
;;
;;      In .emacs.el, by setting ('font . fontset-name) in 
;;          'default-frame-alist
;;      or
;;          'initial-frame-alist
;;      font "fontset-name" should be used.
;;      
;;      To change font on current frame, please use 'set-frame-font.
;;
;;  3. create your own fontset
;;      If you want to create another fontset, please use  new function
;;          (carbon-font-create-fontset fontset size list)
;;              fontset : fontset name(striings)
;;              size : size or list of size that you want to create
;;              list : alist of encodings and font family name
;;    
;;      example : courier and $B%R%i%.%N4]%4%7%C%/(B(hiragino maru gothic)
;;
;;          (setq carbon-font-encode-family-list-courier
;;              '((ascii . "courier")
;;              (japanese-jisx0208 . "$B%R%i%.%N4]%4(B*")
;;              (katakana-jisx0201 . "$B%R%i%.%N4]%4(B*")
;;              (chinese-gb2312 . "$A;*$BJ8$(0E*$BBN(B*")
;;              (chinese-big5-1 . "$BQ6$(0E*(B*")
;;              (korean-ksc5601 . "applegothic*")))
;;
;;          (carbon-font-create-fontset "courier"
;;                                      carbon-font-defined-sizes
;;                                      carbon-font-encode-family-list-courier)
;;
;;      Then, you can get new fontsets "fontset-courier", that have sizes
;;      from 7 to 24 point.
;;
;;  4. Supported encodings on Carbon Emacs
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;       And also "mac-roman" is described 
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;----------------------------------------------------------------------
;;
;; 1. Introduction
;; 1.1. $B$3$N%U%!%$%k$NCf?H(B
;;	$B$3$N%U%!%$%k$O!"(Bcarbon emacs on Mac OSX $B$G!"(B2$B%P%$%HJ8;z$H!"(Bascii$B$r(B
;;	1:2$B$NI}$G(B($B=j0bEyI}(B)$B$G!"I=<($9$k$?$a$N(Bfontset$BDj5A$NNc$r<($7$F$$$^$9!#(B
;;
;;	$BDj5A$7$F$$$k$N$O!"2<5-$N(B4$B<o$NJ8;z%;%C%H$G$9!#(B
;;	    hiraginomaru    Monaco + $B%R%i%.%N4]%4(B
;;	    hiraginokaku    Monaco + $B%R%i%.%N3Q%4(B
;;	    osaka           Monaco + osaka
;;	    hiraginomin     Courier  + $B%R%i%.%NL@D+(B
;;
;;	    point 7,8,9,10,12,14,16,18,20,24 $B$N%5%$%:(B
;;
;;	$B$rDj5A$7$F$$$^$9!#$D$^$j!"$3$N%U%!%$%k$G$O!"(B40$B<o$N!"(Bfontset $B$rDj5A(B
;;	$B$7$F$$$k$3$H$K$J$j$^$9!#(B
;;      default$B$N%5%$%:$O!"(B12$B$G$9!#(B
;; 
;; 1.2. $BF0:n4D6-(B
;;	carbon emacs $B$O!"(B2005-06-01 $B0J9_$N(BCVS$B$+$iF~<j$7$?J*$r8f;H$$$/$@$5$$!#(B
;;	$B$=$l0JA0$NJ*$O!"(B.emacs$BFI$_9~$_$N;~$K%(%i!<$K$J$k2DG=@-$,$"$j$^$9!#(B
;;
;;	$B<o!9$JCN7C$r<x$1$F$/$@$5$C$?!"(Bmac-emacsen ML $B$d(B 2ch mac de emacs$B2q5D(B
;;	$B<<$NJ}!9$K46<U$7$^$9!#(B
;;
;; 2. Usage($B;H$$J}(B)
;;  2.1. Install
;;      $B2<5-(B2$B$D$N%U%!%$%k$r%m!<%I%Q%9$NDL$C$?$H$3$m$KCV$$$F2<$5$$!#(B
;;          carbon-font.el (this file)
;;          fixed-width-fontset.el
;;
;;  2.2. load package
;;      (if (eq window-system 'mac) (require 'carbon-font))
;;      $B$H$7$F$/$@$5$$!#(B
;;
;;  2.3. set fontset
;;      $B<!$N$h$&$K;XDj$7$?>l9g!"(B
;;          "fontset-hiraginokaku"
;;      $B%R%i%.%N3Q%4%7%C%/$N(B12point $B$,;H$o$l$^$9!#(B
;;
;;      $B$3$l0J30$N%5%$%:$r;H$$$?$$>l9g$O!"(B
;;            "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru"
;;      $B$NMM$K!"%U%k(BXLFD$B%M!<%`$r;XDj$7$F$/$@$5$$!#(B
;;
;;  2.3.1. emacs lisp $B$KFk@w$NL5$$J}$N0Y$K(B
;;
;;      $B%G%U%)%k%H$G;H$&%U%)%s%H%;%C%H$N;XDj$9$k>l9g$O0J2<$N$h$&$K(B
;;      fixed-width-set-default-fontset $B$r;H$C$F$/$@$5$$!#(B
;;         12point$B$r;H$&>l9g(B
;;             (fixed-width-set-default-fontset "fontset-hiraginokaku")
;;         12point$B0J30$N%5%$%:$r;H$&>l9g(B
;;             (fixed-width-set-default-fontset
;;                  "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginokaku")
;;
;;      $B:G=i$N%U%l!<%`$@$1!"JL$N(Bfontset$B$r;H$$$?$$>l9g$O!"(B
;;          (add-to-list 'initial-frame-alist '(font . "fontset-hiraginomaru"))
;;      $B$d!"(B
;;          (add-to-list
;;           'initial-frame-alist
;;           '(font . "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru"))
;;      $BEy$H$7$F2<$5$$!#(B
;;
;;      $B:#%+!<%=%k$NM-$k%U%l!<%`$@$1!"%U%)%s%H$rJQ99$7$?$$>l9g$O!"(B
;;          (set-frame-font "fontset-hiraginomaru")
;;      $B$d(B
;;          (set-frame-font
;;           "-*-*-medium-r-normal--8-*-*-*-*-*-fontset-hiraginomaru")
;;      $B$H$7$F$/$@$5$$!#(B
;;
;; 2.3.2. emacs lisp $B$rM}2r$7$F5o$i$l$kJ}$K(B
;;
;;      .emacs.el $B$G(B
;;          'default-frame-alist
;;      $B5Z$S(B
;;          'initial-frame-alist
;;      $B$K!"(B('font . fontset-name) $B$r@_Dj$9$k$3$H$K$h$j!"(Bfontset-name $B$,3F!9(B
;;      $BM-8z$K$J$j$^$9!#(B
;;      $BKt!"8=:_$N%U%l!<%`$N%U%)%s%H$rJQ99$9$k$K$O!"(Bset-frame-font $B$r$*;H$$2<$5$$!#(B
;;
;;  3. $BJL$N%U%)%s%H%;%C%H$r:n$j$?$$>l9g(B
;;      $BJL$NAH9g$;$N(B fontset $B$r@_Dj$7$?$$>l9g!#(B($B?75!G=(B)
;;
;;      (carbon-font-create-fontset fontset size list) $B$r;H$C$F$/$@$5$$!#(B
;;          fontset : fontset $B$NL>A0(B(striings)
;;          size : $B@_Dj$7$?$$%5%$%:!"Kt$O%5%$%:$N%j%9%H(B
;;          list : $B%(%s%3!<%G%#%s%0$H%U%)%s%H$N%U%!%_%j!<%M!<%`$NO"A[%j%9%H(B
;;         
;;      $BNc$($P!"(Bcourier $B$K(B $B%R%i%.%N4]%4%7%C%/$rAH$_9g$o$;$?$$>l9g(B
;;
;;      (setq carbon-font-encode-family-list-courier
;;        '((ascii . "courier")
;;          (japanese-jisx0208 . "$B%R%i%.%N4]%4(B*")
;;          (katakana-jisx0201 . "$B%R%i%.%N4]%4(B*")
;;          (chinese-gb2312 . "$A;*$BJ8$(0E*$BBN(B*")
;;          (chinese-big5-1 . "$BQ6$(0E*(B*")
;;          (korean-ksc5601 . "applegothic*")))
;;
;;      $BEy$HDj5A$7$F$*$$$F!"(B    
;;         
;;      (carbon-font-create-fontset "courier"
;;                                  carbon-font-defined-sizes
;;                                  carbon-font-encode-family-list-courier)
;;
;;      $B$rI>2A$9$l$P!"(B7$B!A(B24 $B$^$G$N%5%$%:$N(B fontset $B$,!"(Bfontset-courier $B$H$$$&L>A0$G(B
;;      $BDj5A$5$l$^$9!#(B
;;
;;  4. $B8=:_!"(Bcarbon emacs $B$,!"%5%]!<%H$7$F$$$k%(%s%3!<%G%#%s%0(B
;;      `mac-charset-info-alist shows
;;      (("mac-dingbats" 34 nil)
;;       ("adobe-fontspecific" 33 nil)
;;       ("mac-symbol" 33 nil)
;;       ("mac-centraleurroman" 29 mac-centraleurroman)
;;       ("gb2312.1980-0" 25 chinese-iso-8bit)
;;       ("mac-cyrillic" 7 mac-cyrillic)
;;       ("ksc5601.1989-0" 3 korean-iso-8bit)
;;       ("big5-0" 2 chinese-big5)
;;       ("jisx0201.1976-0" 1 japanese-shift-jis)
;;       ("jisx0208.1983-sjis" 1 japanese-shift-jis)
;;       ("mac-roman" 0 mac-roman))
;;
;;      "mac-roman" $B$O!"2<5-$N$h$&$K(B3$B$D$N%(%s%3!<%G%#%s%0$r4^$s$G$$$^$9!#(B
;;      ;; Create a fontset that uses mac-roman font.  With this fontset,
;;      ;; characters decoded from mac-roman encoding (ascii, latin-iso8859-1,
;;      ;; and mule-unicode-xxxx-yyyy) are displayed by a mac-roman font.
;;
;;                                                  T.Hiromatsu
;;                                                  matsuan@users.sourceforge.jp

;;
;; fontset section
;;

(require 'fixed-width-fontset)

(defvar fixed-width-encode-reg-alist
  '((japanese-jisx0208 . "jisx0208.*")
    (katakana-jisx0201 . "jisx0201.*")
    (japanese-jisx0212 . "iso10646-*")
    (thai-tis620 . "iso10646-*")
    (chinese-gb2312 . "gb2312.*")
    (chinese-big5-1 . "big5-*")
    (korean-ksc5601 . "ksc5601.*")
    (latin-iso8859-1 . "mac-roman")
    (latin-iso8859-2 . "mac-centraleurroman")
    (cyrillic-iso8859-5 . "mac-cyrillic")))

(defvar fixed-width-xlfd-template
  "-apple-%s-medium-r-normal--%d-*-*-*-*-*-mac-roman")

(defalias 'fixed-width-create-fontset-func 'create-fontset-from-mac-roman-font)

(defalias 'carbon-font-create-fontset 'fixed-width-create-fontset)

;;
;; fontset definition section
;;

(defvar carbon-font-defined-sizes '(12 7 8 9 10 14 16 18 20 24))

;;
;; hiraginomaru = $B%R%i%.%N4]%4(B + monaco
;;

(defvar carbon-font-encode-family-list-hiraginomaru
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N4]%4(B*")
    (katakana-jisx0201 . "$B%R%i%.%N4]%4(B*")
    (japanese-jisx0212 . "hiragino maru gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . "$A;*ND:ZLe(B*")
    (chinese-big5-1 . ,(if (x-list-fonts "*apple ligothic medium*")
                           "apple ligothic medium*" "$(0\cE*(B*"))
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hiraginomaru"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiraginomaru)

;;
;; hiraginokaku = $B%R%i%.%N3Q%4(B + monaco
;;

(defvar carbon-font-encode-family-list-hiraginokaku
  `((ascii . "monaco")
    (japanese-jisx0208 . "$B%R%i%.%N3Q%4(B*")
    (katakana-jisx0201 . "$B%R%i%.%N3Q%4(B*")
    (japanese-jisx0212 . "hiragino kaku gothic pro")
    (thai-tis620 . "ayuthaya")
    (chinese-gb2312 . ,(if (x-list-fonts "*$A;*NDO8:Z(B*") "$A;*NDO8:Z(B*" "$A;*ND:ZLe(B*"))
    (chinese-big5-1 . "$(0\cE*(B*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "hiraginokaku"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiraginokaku)

;;
;; hiraginomin = $B%R%i%.%NL@D+(B + courier 
;;

(defvar carbon-font-encode-family-list-hiraginomin
  `((ascii . "courier")
    (japanese-jisx0208 . "$B%R%i%.%NL@D+(B*")
    (katakana-jisx0201 . "$B%R%i%.%NL@D+(B*")
    (japanese-jisx0212 . "hiragino mincho pro")
    (chinese-gb2312 . ,(if (x-list-fonts "*$A;*NDKNLe(B*") "$A;*NDKNLe(B*" "$A;*ND:ZLe(B*"))
    (chinese-big5-1 . ,(if (x-list-fonts "*$(0\c+{(B*") "$(0\c+{(B*" "$(0\cE*(B*"))
    (korean-ksc5601 . ,(if (x-list-fonts "*applemyungjo*")
                           "applemyungjo*" "applegothic*"))))

(carbon-font-create-fontset "hiraginomin"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-hiraginomin)

;;
;; osaka = osaka + monaco
;;

(defvar carbon-font-encode-family-list-osaka
  '((ascii . "monaco")
    (japanese-jisx0208 . "osaka")
    (katakana-jisx0201 . "osaka")
    (japanese-jisx0212 . "osaka")
    (chinese-gb2312 . "$A;*ND:ZLe(B*")
    (chinese-big5-1 . "$(0\cE*(B*")
    (korean-ksc5601 . "applegothic*")))

(carbon-font-create-fontset "osaka"
                            carbon-font-defined-sizes
                            carbon-font-encode-family-list-osaka)

;;
;;  $BJQ?t(B section
;;

;;  $B;HMQ$9$k%U%)%s%H%;%C%H$rJQ$($?8e!"<+F0$G!"%\!<%k%I$r%j%9%1!<%k$5$;$k!#(B
;;  $B%\!<%k%I$r%j%9%1!<%k$9$k0Y$N%U%!%/%?!<$NDj5A(B

(defvar fixed-width-scale-alist-hiragino
  '(("7" . 1.15) ("8" . 1.3) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-two-byte-bold
  '(("8" . 1.2) ("9" . 1.25) ("10" . 1.1) ("12" . 1.15)
    ("14" . 1.1) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.15)))

(defvar fixed-width-scale-alist-osaka-normal
  '(("7" . 1.15) ("8" . 1.25) ("9" . 1.35) ("10" . 1.2) ("12" . 1.2)
    ("14" . 1.2) ("16" . 1.25) ("18" . 1.25) ("20" . 1.2) ("24" . 1.2)))

(defvar fixed-width-scale-alist-osaka-bold
  '(("8" . 1.2) ("9" . 1.25) ("10" . 1.1) ("12" . 1.1)
    ("14" . 1.2) ("16" . 1.2) ("18" . 1.2) ("20" . 1.15) ("24" . 1.2)))

(defvar fixed-width-scale-alist-monaco-bold
  '(("7" . 0.8) ("8" . 0.95) ("9" . 0.9) ("10" . 0.8) ("12" . 0.9)
    ("14" . 0.9) ("16" . 0.95) ("18" . 0.9) ("20" . 0.95) ("24" . 0.92)))

(defvar fixed-width-get-scale-alist
  `((".*monaco-bold.*" . ,fixed-width-scale-alist-monaco-bold)
    (".*monaco cy-bold.*" . ,fixed-width-scale-alist-monaco-bold)
    (".*courier-bold.*" . (( "9" . 0.9) ("10" . 0.9)))
    (".*osaka-medium.*" . ,fixed-width-scale-alist-osaka-normal)
    (".*osaka-bold.*" . ,fixed-width-scale-alist-osaka-bold)
    ("^-apple-.*pro w[34]-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-.*pro w[34]-.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-hiragino.*" . ,fixed-width-scale-alist-hiragino)
    ("^-apple-.*-bold-[ri]-normal-.*-gb2312\.1980-0$" .
     ,fixed-width-scale-alist-two-byte-bold)
    ("^-apple-.*-medium-[ri]-normal-.*-gb2312\.1980-0$" .
     ,fixed-width-scale-alist-hiragino)
    ("^-apple-.*-bold-[ri]-normal-.*-big5.*" .
     ,fixed-width-scale-alist-two-byte-bold)
    ("^-apple-.*-medium-[ri]-normal-.*-big5.*" .
     ,fixed-width-scale-alist-hiragino)
    ("^-apple-.*-bold-[ri]-normal-.*-ksc5601.*" .
     ,fixed-width-scale-alist-two-byte-bold)
    ("^-apple-.*-medium-[ri]-normal-.*-ksc5601.*" .
     ,fixed-width-scale-alist-hiragino))
  "ReScale factor alist for each fonts and size.")

(provide 'carbon-font)

;;; carbon-font.el ends here
