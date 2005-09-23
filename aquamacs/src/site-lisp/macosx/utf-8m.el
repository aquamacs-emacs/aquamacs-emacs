;;; -*- coding: iso-2022-7bit -*-
;;; utf-8m.el --- modified UTF-8 encoding for Mac OS X hfs plus volume format

;; Copyright (C) 2004-2005  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: v20050920
;; Keywords: mac, multilingual, Unicode, UTF-8
;; Created: 2004-02-20
;; Compatibility: Mac OS X (Carbon Emacs)
;; URL(jp): http://home.att.ne.jp/alpha/z123/emacs-mac-j.html
;; URL(en): http://home.att.ne.jp/alpha/z123/emacs-mac-e.html

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;;; utf-8m について

;; Mac OS X の HFS+ ファイルシステムのファイル名を読むための
;; 修正 UTF8 エンコーディング (utf-8m) を提供します。
;; ファイル名を読み込む際に正規化方式を変更するので
;; 日本語の濁点・半濁点文字とハングル文字、アクセント付きのラテン文字が
;; 文字化けしないようになります。ファイル名を書き出す際の変換は
;; 考慮していませんが、ファイルシステム側がうまく処理してくれるようです。
;; utf-8m を使用するためには、このファイルを読み込んだのち、
;;
;;   (set-file-name-coding-system 'utf-8m)
;;
;; として下さい。


;;; Code:

;; convert utf-8 (NFD) to utf-8 (NFC) by calling `mac-code-convert-string'.
;; ref. http://lists.gnu.org/archive/html/emacs-devel/2005-07/msg01067.html
(defun utf-8m-post-read-conversion (length)
  "Document forthcoming..."
  (save-excursion ;; the original converter
    (setq length (utf-8-post-read-conversion length)))
  (save-excursion ;; additional conversion (NFD -> NFC)
    (save-restriction
      (narrow-to-region (point) (+ (point) length))
      (let ((str (buffer-string)))
        (delete-region (point-min) (point-max))
        (insert-string
         (decode-coding-string
          (mac-code-convert-string
           (encode-coding-string str 'utf-8) 'utf-8 'utf-8 'NFC)
          'utf-8))
        (- (point-max) (point-min))
        ))))

;; define a coding system (utf-8m)
(make-coding-system
 'utf-8m 4 ?u
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
   (mime-charset . utf-8)
   (coding-category . coding-category-utf-8)
   (valid-codes (0 . 255))
   (pre-write-conversion . utf-8-pre-write-conversion)
   (post-read-conversion . utf-8m-post-read-conversion)
   (translation-table-for-encode . utf-translation-table-for-encode)
   (dependency unify-8859-on-encoding-mode
               unify-8859-on-decoding-mode
               utf-fragment-on-decoding
               utf-translate-cjk-mode)))


(provide 'utf-8m)

;; utf-8m.el ends here.
