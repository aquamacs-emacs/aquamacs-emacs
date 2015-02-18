;; fixed-width-fontset.el -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2005-2007 by T. Hiromatsu <matsuan@users.sourceforge.jp>
;; Version 1_0_6
;; 2007-05-20

;;; Commentary:

;; This package defines fixed-width multilingual fontsets for Emacs on Mac
;; OSX and Win32. Comments, questions and feedback will be sent to an english
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
;;      本プログラムはフリー・ソフトウェアです。
;;      あなたは、Free Software Foundationが公表したGNU 一般公有使用許諾の
;;      「バージョン２」或いはそれ以降の各バージョンの中からいずれかを選択し、
;;      そのバージョンが定める条項に従って本プログラムを
;;      再頒布または変更することができます。
;;
;;      本プログラムは有用とは思いますが、頒布にあたっては、
;;      市場性及び特定目的適合性についての暗黙の保証を含めて、
;;      いかなる保証も行ないません。
;;      詳細についてはGNU 一般公有使用許諾書をお読みください。
;;
;;      GNU一般公有使用許諾は、　
;;      Free Software Foundation,
;;         59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
;;         http://www.gnu.org/licenses/gpl.html
;;      から入手可能です。
;;
;;----------------------------------------------------------------------
;; fixed-width-fontset
;;
;;  1. Introduction
;;
;;      This file provides functions for Emacs on Mac OSX and W32.
;;          create CJK fontset
;;          compensate width of ascii bold font that have different width
;;          from normal font.
;;          compensate width of CJK fonts that we want to use 2 times
;;          width of ascii font.
;;
;;      This file is required by carbon-font.el or ntemacs-font.el.
;;
;;  2. installation
;;
;;      please put in this file to the folder on load-path.
;;
;;                                  2005-09-13      Takashi Hiromatsu

;;
;; create fontset functions section
;;

(defun fixed-width-create-encode-family-reg-list (list)
  (mapcar (function
           (lambda (s)
             (let ((reg (cdr (assoc (car s) fixed-width-encode-reg-alist))))
               (cons (car s) (cons (cdr s) reg)))))
          list))

(defun fixed-width-set-fontset-font (fontset list)
  (dolist (elt list) (set-fontset-font fontset (car elt) (cdr elt))))

(defun fixed-width-create-fontset (fontset size list)
  (if (listp size)
      (dolist (elt size) (fixed-width-create-fontset fontset elt list))
    (let* ((asc-font (assoc 'ascii list))
           (asc-xlfd (format fixed-width-xlfd-template (cdr asc-font) size))
           (asc-fontset (fixed-width-create-fontset-func asc-xlfd nil fontset))
           (new-list (delete asc-font list))
           (ecd-fml-reg (fixed-width-create-encode-family-reg-list new-list)))
      (fixed-width-set-fontset-font asc-fontset ecd-fml-reg))))

;;
;; variables
;;

(defvar fixed-width-rescale t)

(defvar fixed-width-fontset-template "-*-*-medium-r-normal--%d-*-*-*-*-*-fontset-%s")

;; font-width-compensation function section
;; カレントフレームで使われているフォントの、リスケールファクターを、
;; fixed-width-scale-alist から、取得する。

(defun fixed-width-append-factor (&optional frame)
  "取得した rescale factor で、face-font-rescale-alist を書き換える。"
  (let* ((res-alist (copy-alist (frame-parameter frame 'face-font-rescale-alist)))
         (frm-font (frame-parameter frame 'font))
         (def-font (cdr (assoc 'font default-frame-alist)))
         (fontset (or frm-font def-font "fontset-default"))
         (asc (if (x-list-fonts fontset) fontset (fontset-font fontset ?a)))
         (size (aref (x-decompose-font-name asc) xlfd-regexp-pixelsize-subnum)))
    (dolist (elt fixed-width-get-scale-alist)
      (let* ((font (car elt))
             (new (if fixed-width-rescale (or (cdr (assoc size elt)) 1.0) 1.0))
             (old (assoc font res-alist)))
        (if old (setcdr old new) (add-to-list 'res-alist (cons font new)))))
    (setq face-font-rescale-alist res-alist)))

(defun fixed-width-set-default-fontset (fontset)
  "Set default font of default-frame-alist"
  (let ((old (assoc 'font default-frame-alist)))
    (if old (setcdr old fontset)
      (add-to-list 'default-frame-alist (cons 'font fontset)))))

(defun fixed-width-set-fontset (fontset &optional size)
  "Set FONTSET and SIZE to `default-frame-alist' and `frame-parameter' of
current frame as `font'. if size is nil, default size of FONTSET will be used.
To get available fontset, use `fontset-list'."
  (let* ((fnt-xlfd (fontset-font (format "fontset-%s" fontset) ?a))
         (sz-asc (aref (x-decompose-font-name fnt-xlfd) xlfd-regexp-pixelsize-subnum))
         (sz (or size (string-to-int sz-asc)))
         (fnt (format fixed-width-fontset-template sz fontset))
         (old (assoc 'font default-frame-alist)))
    (if old (setcdr old fnt)
      (add-to-list 'default-frame-alist (cons 'font fnt)))
    (if (current-frame-configuration) (set-frame-font fnt))))

(defun fixed-width-make-frame-param (frame)
  "Add face-font-rescale-alist to frame parameters as frame local"
  (modify-frame-parameters frame
                           `((face-font-rescale-alist . ,face-font-rescale-alist))))
  
;; add hook section
;; フォントが変更された場合にフックをかけて、
;; fixed-width-append-factor を起動する。

(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (fixed-width-make-frame-param frame)
             (make-variable-frame-local 'face-font-rescale-alist)
             (fixed-width-append-factor frame)))

(add-hook 'after-setting-font-hook '(lambda () (fixed-width-append-factor nil)))

(add-hook 'emacs-startup-hook
          '(lambda ()
             (set-frame-font (or (cdr (assoc 'font initial-frame-alist))
                                 (cdr (assoc 'font default-frame-alist))
                                 (frame-parameter nil 'font)
                                 "fontset-default"))))

;; 初期化処理
;; fixed-width-fontset を load した時に、既に存在している frame の処理。
;; 先に、face-font-rescale-alist を、frame-parameter に設定した後に、
;; make-valiable-rame-local で、各 frame に束縛する

(dolist (elt (frame-list)) (fixed-width-make-frame-param elt))

(make-variable-frame-local 'face-font-rescale-alist)

(provide 'fixed-width-fontset)

;;; fixed-width-fontset.el ends here
