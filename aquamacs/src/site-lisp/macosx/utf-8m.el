;;; -*- coding: iso-2022-7bit -*-
;;; utf-8m.el --- modified UTF-8 encoding for Mac OS X hfs plus volume format

;; Copyright (C) 2004-2008  Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: utf-8m.el,v 1.6 2009/03/07 04:44:21 davidswelt Exp $
;; Keywords: mac, multilingual, Unicode, UTF-8
;; Created: 2004-02-20
;; Compatibility: Emacs 22
;; URL(jp): http://homepage.mac.com/zenitani/emacs-j.html
;; URL(en): http://homepage.mac.com/zenitani/emacs-e.html

;; Contributed by Eiji Honjoh and Carsten Bormann

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a modified utf-8 encoding (utf-8m) for Mac OSX
;; hfs plus volume format. By setting utf-8m as the file-name-coding-system,
;; emacs can read the following characters in filenames.
;; 
;;  * Japanese Kana characters with Dakuten/Han-Dakuten signs
;;  * Korean Hangul characters
;;  * Latin characters with diacritical marks (accents, umlauts, tilde, etc.)
;;
;; Note that utf-8m does not restore the above characters when
;; it exports the filenames. Fortunately, it seems that the filesystem
;; knows how to deal with such invalid filenames.
;;
;; In order to use, add the below line to your .emacs file.
;; 
;;   (set-file-name-coding-system 'utf-8m)
;;

;;; utf-8m $B$K$D$$$F(B

;; Mac OS X $B$N(B HFS+ $B%U%!%$%k%7%9%F%`$N%U%!%$%kL>$rFI$`$?$a$N(B
;; $B=$@5(B UTF8 $B%(%s%3!<%G%#%s%0(B (utf-8m) $B$rDs6!$7$^$9!#(B
;; $B%U%!%$%kL>$rFI$_9~$`:]$K@55,2=J}<0$rJQ99$9$k$N$G(B
;; $BF|K\8l$NByE@!&H>ByE@J8;z$H%O%s%0%kJ8;z!"%"%/%;%s%HIU$-$N%i%F%sJ8;z$,(B
;; $BJ8;z2=$1$7$J$$$h$&$K$J$j$^$9!#%U%!%$%kL>$r=q$-=P$9:]$NJQ49$O(B
;; $B9MN8$7$F$$$^$;$s$,!"%U%!%$%k%7%9%F%`B&$,$&$^$/=hM}$7$F$/$l$k$h$&$G$9!#(B
;; utf-8m $B$r;HMQ$9$k$?$a$K$O!"$3$N%U%!%$%k$rFI$_9~$s$@$N$A!"(B
;;
;;   (set-file-name-coding-system 'utf-8m)
;;
;; $B$H$7$F2<$5$$!#(B


;;; Code:


;; Japanese Kana characters with Dakuten/Han-Dakuten signs

(defvar utf-8m-fix-kana1-alist
  (string-to-list "$B$+$-$/$1$3$5$7$9$;$=$?$A$D$F$H$O$R$U$X$[%+%-%/%1%3%5%7%9%;%=%?%A%D%F%H%O%R%U%X%[!3!5(B"))
(defvar utf-8m-fix-kana2-alist (string-to-list "$B$O$R$U$X$[%O%R%U%X%[(B"))
(defvar utf-8m-fix-kana3-alist (string-to-list "$B%o%p%q%r(B"))

(defun utf-8m-post-read-kana-conversion (length)
  "Document forthcoming..."
  (save-excursion
    (while (not (eobp))
      (let ((ch1 (char-before))
            (ch2 (char-after)))
        (cond
         ((= ch2 ?$,2>y(B) ;; 302969 or 12441
          (cond
           ((memq ch1 utf-8m-fix-kana1-alist)
            (delete-char -1)
            (delete-char 1)
            (insert (+ ch1 1))
            (setq length (- length 1))
            )
;;            ((memq ch1 utf-8m-fix-kana3-alist)
;;             (delete-char -1)
;;             (delete-char 1)
;;             (insert (+ ch1 1244))
;;             (setq length (- length 1))
;;             )
           ((= ch1 ?$B%&(B)
            (delete-char -1)
            (delete-char 1)
            (insert ?$B%t(B)
            (setq length (- length 1))
            )))
          ((= ch2 ?$,2>z(B) ;; 302970 or 12442
           (cond
            ((memq ch1 utf-8m-fix-kana2-alist)
             (delete-char -1)
             (delete-char 1)
             (insert (+ ch1 2))
             (setq length (- length 1))
             ))))
        (if (not (eobp))(forward-char))
        )))
  length)


;;  Latin characters with diacritical marks

(defvar utf-8m-fix-latin-alist
  '(
    (?$,1%@(B . ( ;; grave 332480 (e22) or 768 (e23)
           (?A . ?,A@(B) (?E . ?,AH(B) (?I . ?,AL(B) (?O . ?,AR(B) (?U . ?,AY(B)
           (?a . ?,A`(B) (?e . ?,Ah(B) (?i . ?,Al(B) (?o . ?,Ar(B) (?u . ?,Ay(B)
           ))
    (?$,1%A(B . ( ;; acute 332481, 769
           (?A . ?,AA(B) (?E . ?,AI(B) (?I . ?,AM(B) (?O . ?,AS(B) (?U . ?,AZ(B) (?Y . ?,b](B)
           (?C . ?$,1 &(B) (?L . ?$,1 Y(B) (?N . ?$,1 c(B) (?R . ?$,1 t(B) (?S . ?$,1 z(B) (?Z . ?$,1!9(B)
           (?a . ?,Aa(B) (?e . ?,Ai(B) (?i . ?,Am(B) (?o . ?,As(B) (?u . ?,Az(B) (?y . ?,b}(B)
           (?c . ?$,1 '(B) (?l . ?$,1 Z(B) (?n . ?$,1 d(B) (?r . ?$,1 u(B) (?s . ?$,1 {(B) (?z . ?$,1!:(B)
           ))
    (?$,1%B(B . ( ;; circumflex 332482, 770
           (?A . ?,AB(B) (?E . ?,AJ(B) (?I . ?,AN(B) (?O . ?,AT(B) (?U . ?,A[(B)
           (?a . ?,Ab(B) (?e . ?,Aj(B) (?i . ?,An(B) (?o . ?,At(B) (?u . ?,A{(B)
           ))
    (?$,1%C(B . ( ;; tilda 332483, 771
           (?A . ?,bC(B) (?N . ?,bQ(B) (?O . ?,bU(B)
           (?a . ?,bc(B) (?n . ?,bq(B) (?o . ?,bu(B)
           ))
    (?$,1%D(B . ( ;; macron 332484, 772
           (?A . ?$,1  (B) (?E . ?$,1 2(B) (?I . ?$,1 J(B) (?O . ?$,1 l(B) (?U . ?$,1!*(B)
           (?a . ?$,1 !(B) (?e . ?$,1 3(B) (?i . ?$,1 K(B) (?o . ?$,1 m(B) (?u . ?$,1!+(B)
           ))
    (?$,1%G(B . ( ;; dot above 332487, 775
           (?E . ?$,1 6(B) (?Z . ?$,1!;(B)
           (?e . ?$,1 7(B) (?z . ?$,1!<(B)
           ))
    (?$,1%H(B . ( ;; umlaut 332488, 776
           (?A . ?,AD(B) (?E . ?,AK(B) (?I . ?,AO(B) (?O . ?,AV(B) (?U . ?,A\(B) (?Y . ?,b>(B)
           (?a . ?,Ad(B) (?e . ?,Ak(B) (?i . ?,Ao(B) (?o . ?,Av(B) (?u . ?,A|(B) (?y . ?,A(B)
           ))
    (?$,1%J(B . ( ;; angstrom / ring above 332490, 778
           (?A . ?,AE(B) (?U . ?$,1!.(B)
           (?a . ?,Ae(B) (?u . ?$,1!/(B)
           ))
    (?$,1%K(B . ( ;; double accute 332491, 779
           (?O . ?$,1 p(B) (?U . ?$,1!0(B)
           (?o . ?$,1 q(B) (?u . ?$,1!1(B)
           ))
    (?$,1%L(B . ( ;; caron 332492, 780
           (?C . ?$,1 ,(B) (?D . ?$,1 .(B) (?E . ?$,1 :(B) (?L . ?$,1 ](B) (?N . ?$,1 g(B)
           (?R . ?$,1 x(B) (?S . ?$,1! (B) (?T . ?$,1!$(B) (?Z . ?$,1!=(B)
           (?c . ?$,1 -(B) (?d . ?$,1 /(B) (?e . ?$,1 ;(B) (?l . ?$,1 ^(B) (?n . ?$,1 h(B)
           (?r . ?$,1 y(B) (?s . ?$,1!!(B) (?t . ?$,1!%(B) (?z . ?$,1!>(B)
           ))
    (?$,1%g(B . ( ;; cedilla 332519, 807
           (?C . ?,bG(B) (?G . ?$,1 B(B) (?K . ?$,1 V(B) (?L . ?$,1 [(B) (?N . ?$,1 e(B) (?R . ?$,1 v(B)
           (?c . ?,bg(B) (?g . ?$,1 C(B) (?k . ?$,1 W(B) (?l . ?$,1 \(B) (?n . ?$,1 f(B) (?r . ?$,1 w(B)
           ))
    (?$,1%h(B . ( ;; ogonek 332520, 808
           (?A . ?$,1 $(B) (?E . ?$,1 8(B) (?I . ?$,1 N(B) (?U . ?$,1!2(B)
           (?a . ?$,1 %(B) (?e . ?$,1 9(B) (?i . ?$,1 O(B) (?u . ?$,1!3(B)
           ))
    ))

(defun utf-8m-post-read-latin-conversion (length)
  "Document forthcoming..."
  (save-excursion
    (dotimes (i length) (forward-char))
    (let ((accent_char nil)
          (accent_count 0))
      (while (not (bobp))
        (let ((ch (char-before)))
          (cond
           ((and (= accent_count 1)
                 (assoc accent_char utf-8m-fix-latin-alist)
                 (assoc ch (cdr (assoc accent_char utf-8m-fix-latin-alist)))
                 )
            (delete-char -1)
            (delete-char 1)
            (insert
             (cdr (assoc ch
                         (cdr (assoc accent_char utf-8m-fix-latin-alist))
                         )))
            (setq length (- length 1))
            (setq accent_count 0)
            )
           ((assoc ch utf-8m-fix-latin-alist)
            (setq accent_char ch)
            (setq accent_count (+ accent_count 1))
            )
           (t (setq accent_count 0))
           )
        (if (not (bobp))(backward-char))
        )
      )))
  length)


;; Korean Hangul characters
;; ref. http://www.unicode.org/reports/tr15/#Hangul

(defun utf-8m-post-read-hangul-conversion (length)
  "Document forthcoming..."
  (save-excursion
    (let* ((ch1 nil)
           (ch2 nil)
           (sbase #xac00)
           (lbase #x1100)
           (vbase #x1161)
           (tbase #x11a7)
           (lcount 19)
           (vcount 21)
           (tcount 28)
           (ncount (* vcount tcount)) ; 588
           (scount (* lcount ncount)) ; 11172
           (lindex nil)
           (vindex nil)
           (sindex nil)
           (tindex nil))
      (if (not (eobp)) (forward-char))
      (if (not (eobp)) (setq ch1 (encode-char (char-before) 'ucs)))
      (while (not (eobp))
        (setq ch2 (encode-char (char-after) 'ucs))
;       (message "ch1:%X ch2:%X" ch1 ch2)
        (setq lindex (- ch1 lbase))
        (setq vindex (- ch2 vbase))
        (setq sindex (- ch1 sbase))
        (setq tindex (- ch2 tbase))
        (if (and (>= lindex 0)(< lindex lcount)
                 (>= vindex 0)(< vindex vcount))
            (progn
;             (message "first loop")
              (setq ch1 (+ sbase (* (+ (* lindex vcount) vindex) tcount)))
              (delete-char -1)
              (delete-char 1)
              (ucs-insert ch1)
              (setq length (- length 1))
              )
          (if (and (>= sindex 0)(< sindex scount)
                   (= (% sindex tcount) 0)
                   (>= tindex 0)(< tindex tcount))
              (progn
;               (message "second loop")
                (setq ch1 (+ ch1 tindex))
                (delete-char -1)
                (delete-char 1)
                (ucs-insert ch1)
                (setq length (- length 1))
                )
            (progn
              (setq ch1 ch2)
              (if (not (eobp))(forward-char))
              )
            ))
        )))
  length)


;; ---- post-read-converters ----


;; Emacs 22 version
(defun utf-8m-e22-post-read-conversion (length)
  "Document forthcoming..."
  (save-excursion
    (setq length (utf-8-post-read-conversion length)))
  (save-excursion
    (setq length (utf-8m-post-read-kana-conversion length)))
  (save-excursion
    (setq length (utf-8m-post-read-hangul-conversion length)))
  (save-excursion
    (setq length (utf-8m-post-read-latin-conversion length)))
  length)

;; Emacs 22 version (mac-only)
;; convert utf-8 (NFD) to utf-8 (NFC) by calling `mac-code-convert-string'.
;; ref. http://lists.gnu.org/archive/html/emacs-devel/2005-07/msg01067.html
(defun utf-8m-e22-mac-post-read-conversion (length)
  "Document forthcoming..."
  (save-excursion ;; the original converter
    (setq length (utf-8-post-read-conversion length)))
  (save-excursion ;; additional conversion (NFD -> NFC)
    (save-restriction
      (narrow-to-region (point) (+ (point) length))
      (let ((str (buffer-string)))
        (delete-region (point-min) (point-max))
        (insert
         (decode-coding-string
          (or (mac-code-convert-string
	       (or (encode-coding-string str 'utf-8) str) 
	       'utf-8 'utf-8 'NFC) 
	      str)
	      'utf-8))
        (- (point-max) (point-min))
        ))))

;; Emacs 23 version
(defun utf-8m-e23-post-read-conversion (length)
  "Document forthcoming..."
  (save-excursion
    (setq length (utf-8m-post-read-kana-conversion length)))
  (save-excursion
    (setq length (utf-8m-post-read-hangul-conversion length)))
  (save-excursion
    (setq length (utf-8m-post-read-latin-conversion length)))
  length)


;; ---- define a coding system (utf-8m) ----

(cond

 ;; Emacs 22
 ((equal emacs-major-version 22)
  (make-coding-system
   'utf-8m 4 ?U
   "modified UTF-8 encoding for Mac OS X hfs plus volume format."
   '(ccl-decode-mule-utf-8 . ccl-encode-mule-utf-8)
   `((safe-charsets
      ascii
      eight-bit-control
      eight-bit-graphic
      latin-iso8859-1
      mule-unicode-0100-24ff
      mule-unicode-2500-33ff
      mule-unicode-e000-ffff
      ,@(if utf-translate-cjk-mode
            utf-translate-cjk-charsets))
     (mime-charset . nil)
     (coding-category . coding-category-utf-8)
     (valid-codes (0 . 255))
     (pre-write-conversion . utf-8-pre-write-conversion)
     ; (pre-write-conversion . utf-8m-pre-write-conversion)
     ; (post-read-conversion . utf-8-post-read-conversion)
     ,(if (functionp 'mac-code-convert-string)
          '(post-read-conversion . utf-8m-e22-mac-post-read-conversion)
        '(post-read-conversion . utf-8m-e22-post-read-conversion))
     (translation-table-for-encode . utf-translation-table-for-encode)
     (dependency unify-8859-on-encoding-mode
                 unify-8859-on-decoding-mode
                 utf-fragment-on-decoding
                 utf-translate-cjk-mode)))
  )

 ;; Emacs 23 (doesn't work)
 (nil ;(equal emacs-major-version 23)
  (define-coding-system 'utf-8m
    "UTF-8 Mac file system encoding."
    :coding-type 'utf-8
    :mnemonic ?U
    :charset-list '(unicode)
    :post-read-conversion 'utf-8m-e23-post-read-conversion)
  )

 ) ;; (cond


;; (set-file-name-coding-system 'utf-8m)

(provide 'utf-8m)

;; utf-8m.el ends here.
