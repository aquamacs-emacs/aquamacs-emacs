;;; css-mode.el --- A minimal CSS mode.
;; Copyright 2002  Lawrence Mitchell
;; Copyright 2005  Alex Schroeder

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; A (very) minimal CSS mode.  Does indentation, some font-locking and
;; nothing more.

;;; Code:

(eval-when-compile
  (defvar comment-quote-nested))

(defvar css-mode-map nil
  "Keymap for `css-mode'.")

(defvar css-mode-indent-depth 4
  "*Depth of indentation.")

(defvar css-mode-font-lock-keywords-1
  (eval-when-compile
    (let ((keywords (regexp-opt
                     '("a" "abbr" "acronym" "address" "applet" "area" "b"
                       "base" "basefont" "bdo" "big" "blockquote" "body" "br"
                       "button" "caption" "center" "cite" "code" "col"
                       "colgroup" "dd" "del" "dfn" "dir" "div" "dl" "dt" "em"
                       "fieldset" "font" "form" "frame" "frameset" "h1" "h2"
                       "h3" "h4" "h5" "h6" "head" "hr" "html" "i" "iframe"
                       "img" "input" "ins" "isindex" "kbd" "label" "legend"
                       "li" "link" "map" "menu" "meta" "noframes" "noscript"
                       "object" "ol" "optgroup" "option" "p" "param" "pre" "q"
                       "s" "samp" "script" "select" "small" "span" "strike"
                       "strong" "style" "sub" "sup" "table" "tbody" "td"
                       "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "u"
                       "ul" "var"))))
      (list
       (list
        (concat "\\b\\(" keywords "\\)[]{. \t]")
        1 'font-lock-keyword-face))))
  "Normal level higlighting for `css-mode'.")

(defvar css-mode-font-lock-keywords-2
  (append css-mode-font-lock-keywords-1
          (eval-when-compile
            (let ((keywords (regexp-opt
                             '("azimuth" "background" "background-attachment" "background-color"
                               "background-image" "background-position" "background-repeat"
                               "border" "border-collapse" "border-color" "border-spacing"
                               "border-style" "border-top" "border-right" "border-bottom"
                               "border-left" "border-top-color" "border-right-color"
                               "border-bottom-color" "border-left-color" "border-top-style"
                               "border-right-style" "border-bottom-style" "border-left-style"
                               "border-top-width" "border-right-width" "border-bottom-width"
                               "border-left-width" "border-width" "bottom" "caption-side"
                               "clear" "clip" "color" "content" "counter-increment"
                               "counter-reset" "cue" "cue-after" "cue-before" "cursor"
                               "direction" "display" "elevation" "empty-cells" "float" "font"
                               "font-family" "font-size" "font-size-adjust" "font-stretch"
                               "font-style" "font-variant" "font-weight" "height" "left"
                               "letter-spacing" "line-height" "list-style" "list-style-image"
                               "list-style-position" "list-style-type" "margin" "margin-top"
                               "margin-right" "margin-bottom" "margin-left" "marker-offset"
                               "marks" "max-height" "max-width" "min-height" "min-width"
                               "orphans" "outline" "outline-color" "outline-style"
                               "outline-width" "overflow" "padding" "padding-top"
                               "padding-right" "padding-bottom" "padding-left" "page"
                               "page-break-after" "page-break-before" "page-break-inside"
                               "pause" "pause-after" "pause-before" "pitch" "pitch-range"
                               "play-during" "position" "quotes" "richness" "right" "size"
                               "speak" "speak-header" "speak-numeral" "speak-punctuation"
                               "speech-rate" "stress" "table-layout" "text-align"
                               "text-decoration" "text-indent" "text-shadow" "text-transform"
                               "top" "unicode-bidi" "vertical-align" "visibility" "voice-family"
                               "volume" "white-space" "widows" "width" "word-spacing"
                               "z-index"))))
              (list
               (list
                (concat "\\<\\("
                        keywords
                        "\\)\\>[ \t]*:")
                1 'font-lock-type-face)))))
  "Gaudy level highlighting for `css-mode'.")

;; this is too excessive.
;; (defvar css-mode-font-lock-keywords-3
;;   (append css-mode-font-lock-keywords-2
;;           (eval-when-compile
;;             (list
;;              (list
;;               ":[ \t]*\\(.*\\);"
;;               1 'font-lock-variable-name-face)
;;              (list
;;               ":[ \t]*\\(rgb\\)([ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*,[ \t]*[0-9]+[ \t]*)"
;;               1 'font-lock-function-name-face 'prepend))))
;;   "Incredibly over-the-top highlighting for `css-mode'.")
              
(defvar css-mode-font-lock-keywords css-mode-font-lock-keywords-1
  "Default expressions to highlight in `css-mode'.")

(defvar css-mode-syntax-table nil
  "Syntax table for `css-mode'.")

(defun css-mode ()
  "Major mode for editing Cascading StyleSheets."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'css-mode
        mode-name "CSS")
  (use-local-map css-mode-map)
  ;; set up syntax table.
  (if css-mode-syntax-table
      ()
    (setq css-mode-syntax-table
          (let ((table (make-syntax-table)))
            ;; comment characters.
            (modify-syntax-entry ?/  ". 14" table)
            (modify-syntax-entry ?*  ". 23"   table)
            table)))
  (set-syntax-table css-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'comment-quote-nested)
  (setq comment-quote-nested t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-style)
  (setq comment-style 'extra-line)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'css-fill)
  ;; (make-local-variable 'comment-indent-function)
  ;; (setq comment-indent-function 'css-mode-indent-comment)
  (setq indent-line-function 'css-mode-indent-line)
  (setq font-lock-defaults '((css-mode-font-lock-keywords
                              css-mode-font-lock-keywords-1
                              css-mode-font-lock-keywords-2))))

;; set up keymap
(if css-mode-map
    ()
  (setq css-mode-map (make-sparse-keymap))
  (define-key css-mode-map (kbd "RET") 'css-mode-newline-and-indent)
  (define-key css-mode-map [(tab)] 'css-mode-indent-line)
  (define-key css-mode-map [(})] 'css-mode-electric-insert-close-brace))

(defun css-mode-calc-indent-level ()
  "Calculate the indent level for the current line."
  ;; This counts indentation level based on parenthesis (the first
  ;; value in the list returned by parse-partial-sexp).  It then
  ;; corrects as follows: If we're on a line with a closing
  ;; parenthesis, outdent.  If we're inside a comment, indent by
  ;; three, unless the line starts with the closing comment sequence.
  (let* ((indent-data (parse-partial-sexp (point-min)
                                          (line-beginning-position)))
         (indent (car indent-data))
         (in-comment (nth 4 indent-data))
         close-block
         close-comment
         pos)
    (save-excursion
      (back-to-indentation)
      (setq close-block (looking-at "}")
            close-comment (looking-at "\\*/")))
    (when close-block
      (setq indent (1- indent)))
    (setq pos (* indent css-mode-indent-depth))
    (if (and in-comment (not close-comment))
        (+ 3 pos)
      pos)))
      
(defun css-mode-indent-line (&optional indent)
  "Indent the current line.

If optional INDENT is non-nil, use that instead of calculating the
indent level."
  (interactive)
  (let ((indent (or indent (css-mode-calc-indent-level)))
        pos)
    (save-excursion
      (back-to-indentation)
      (delete-region (point-at-bol) (point))
      (indent-to indent)
      (setq pos (point)))
    (when (> pos (point))
      (goto-char pos)))); move only forward after indenting


(defun css-mode-newline-and-indent ()
  "Insert a newline and indent."
  (interactive)
  (newline)
  (css-mode-indent-line))

(defun css-mode-electric-insert-close-brace ()
  "Insert a closing brace }."
  (interactive)
  (insert "}")
  (css-mode-indent-line)
  (forward-char))

(defun css-fill (&rest ignore)
  "Lay out CSS expressions."
  (interactive)
  (condition-case data
      (progn
        (backward-up-list 1)
        (forward-char 1)
        (newline-and-indent)
        (catch 'done
          (let ((start (point)))
            (while (re-search-forward "\n\\|;")
              (let* ((data (save-excursion
                             (parse-partial-sexp start (point))))
                     (depth (nth 0 data))
                     (in-string (nth 3 data))
                     (in-comment (nth 4 data)))
                (when (< depth 0)
                  (throw 'done t))
                (when (and (= depth 0)
                           (not (or in-string in-comment)))
                  (if (string= (match-string 0) "\n")
                      (replace-match "")
                    (newline-and-indent))))))))
    (scan-error
     (search-forward "{"))))

(provide 'css-mode)

;;; css-mode.el ends here