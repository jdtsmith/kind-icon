;;; kind-icon.el --- Completion kind icons  -*- lexical-binding: t -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/kind-icon
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.1.0
;; Keywords: completion

;;; Commentary:

;; kind-icon-mode adds an colorful icon or text prefix based on
;; :company-kind for compatible completion UI's.  The "kind" prefix is
;; typically used for differentiating completion candidates such as
;; variables, functions, etc.  It works in one of 2 ways:
;; 
;;  1. For UI's with a "kind-formatter" option, simply set that
;;     function to `kind-icon-formatted'.
;;
;;  2. For UI's without a kind-formatter but which support "affixation
;;     functions" (an Emacs 28 and later completion property), use
;;     `kind-icon-enhance-completion' to wrap the normal
;;     completion-in-region-function.  E.g. (in the completion mode's
;;     hook):
;;
;;     (setq completion-in-region-function
;;        (kind-icon-enhance-completion completion-in-region-function)
;;
;;  3. If your UI supports neither a kind-formatter nor affixation
;;     functions, ask them to do so!
;;
;; Note that icon support requires svg-lib to be installed.
;;
;; The `kind-icon-formatted' function creates, styles, and caches a
;; short-text or icon-based "badge" representing the kind of the
;; candidate.  Icons are by default loaded remotely from the
;; "material" library provided by svg-lib, which is required (unless
;; only short-text badges are desired, see `kind-icon-use-icons').
;; Customize `kind-icon-mapping' to configure mapping between kind and
;; both short-text and icons.

;; kind-icon is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; kind-icon is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:
(defgroup kind-icon nil
  "Completion prefixes from :company-kind."
  :group 'convenience
  :prefix "kind-icon")

(defcustom kind-icon-use-icons t
  "Whether to use icons for prefix display."
  :type 'boolean)

(unless (require 'svg-lib nil 'noerror)
  (setq kind-icon-use-icons nil))

(defcustom kind-icon-mapping ;; adapted from company
  '((array "a" :icon "code-brackets" :face font-lock-type-face)
    (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
    (class "c" :icon "view-grid-plus-outline" :face font-lock-type-face)
    (color "#" :icon "palette" :face success)
    (constant "co" :icon "lock-remove-outline" :face font-lock-constant-face)
    (enum-member "em" :icon "format-list-checks" :face font-lock-builtin-face)
    (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
    (field "fd" :icon "application-braces-outline" :face font-lock-variable-name-face)
    (file "f" :face :icon "file-document-outline" font-lock-string-face)
    (folder "d" :icon "folder" :face font-lock-doc-face)
    (interface "if" :icon "application-brackets-outline" :face font-lock-type-face)
    (keyword "kw" :icon "key-variant" :face font-lock-keyword-face)
    (method "m" :icon "function-variant" :face font-lock-function-name-face)
    (function "f" :icon "function" :face font-lock-function-name-face)
    (module "{" :icon "file-code-outline" :face font-lock-type-face)
    (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
    (operator "op" :icon "plus-minus" :face font-lock-comment-delimiter-face)
    (parameter "pa" :icon "application-variable-outline" :face font-lock-builtin-face)
    (property "pr" :icon "application-parentheses-outline" :face font-lock-variable-name-face)
    (ruler "r" :icon "ruler" :face shadow)
    (snippet "S" :icon "note-text-outline" :face font-lock-string-face)
    (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
    (struct "%" :icon "code-braces" :face font-lock-variable-name-face)
    (text "tx" :icon "script-text-outline" :face shadow)
    (value "v" :icon "plus-circle-outline" :face font-lock-builtin-face)
    (variable "va" :icon "variable" :face font-lock-variable-name-face)
    (t "." :icon "crosshairs-question" :face shadow))
  "Mapping of kinds.
The format should be an alist of type:

   (KIND SHORT-TEXT PLIST)

This information is used to build a prefix for kind KIND.  A
prefix is a propertized string of either an icon or the
SHORT-TEXT (two characters max), depending on the value of
variable `kind-icon-use-icons' and presence of :icon in the
PLIST.  The PLIST is optional and includes keywords :icon and
:face.  :icon is a name of an icon from the material
collection (see `svg-lib'). :face is a face from which the
:foreground face-property is used for the foreground. If
`kind-icon-blend-background' is non-nil, the icon's background
color is automatically computed to lie between the default-face
or frame background color and the foreground color (see
`kind-icon-blend-frac').  If `kind-icon-blend-background' is nil,
the background color is taken from the :face's background in this
map, or, if that is missing or unspecified, from the frame's
background color."
  :link '(url-link "https://materialdesignicons.com")
  :type '(repeat 
	  (list :tag "Mapping"
		(symbol :tag "Kind")
		(string :tag "Short-Text")
		(plist :tag "Icon/Face options"
		       :inline t
		       :options
		       ((:icon (string :tag "Icon Name"
				       :format "%[Preview%] %t: %v"
				       :action kind-icon--preview))
			(:face (face :tag "Face")))))))

(defcustom kind-icon-blend-background t
  "Whether to set a custom background blended from foreground."
  :type 'boolean)

(defcustom kind-icon-blend-frac 0.12
  "Fractional blend between foreground and background colors.
This is used for the prefix background, if
kind-icon-blend-background is non-nil."
  :type 'float)

(defcustom kind-icon-default-face nil
  "The default face to use for coloring.
Normally foreground colors are supplied by the face matching in
`kind-icon-mapping', but if no face is supplied in the mapping,
the foreground color is taken from the foreground of this face,
or (if nil) to the default frame foreground color.  The
background color is also taken from this face, if provided,
otherwise defaulting to the frame background color."
  :type 'face)

(defcustom kind-icon-default-style
  '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 1.0)
  "Default style parameters for building SVG icons.
See `svg-lib-style-compute-default'."
  :type 'plist)

(defun kind-icon--preview (widget _e)
  (let* ((icon (widget-value widget)))
    (message "%S [%s] looks like: %s" icon
	     (pp-to-string (widget- widget))
	     (propertize "**" 'display
			 (apply #'svg-lib-icon
				icon nil kind-icon-default-style)))))

(defsubst kind-icon--rgb-blend (rgb1 rgb2 frac)
  "Return a fractional blend between two colors RGB1 and RGB2.
Each is a 3 element list.  The fractional blend point is the
float FRAC."
  (apply #'color-rgb-to-hex
	 (cl-mapcar (lambda (a b)
		      (+ (* a frac) (* b (- 1.0 frac))))
		    rgb1 rgb2)))

(defsubst kind-icon--metdata-get (metadata type-name)
  (or
   (cdr (assq (intern type-name) metadata))
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))))

(defun kind-icon-formatted (kind)
  "Return a formatted kind badge, either icon or text abbreviation.
Caches this badge as :display-icon in `kind-icon-mapping', and
returns the cached value, if set.  If
`kind-icon-blend-background' is non-nil, computes a blend between
a nominal background color (from either the background property
of `kind-icon-default-face', if set, or frame background color)
and foreground.  For the foreground color, uses the :face
mapping's :foreground color, the `kind-icon-default-face'
foreground, or the default frame foreground, in that order of
priority.  If `kind-icon-blend-background' is nil, the background
is taken from the :face background, `kind-icon-default-face`, or
frame background-color."
  (when-let ((map (assq kind kind-icon-mapping))
	     (plist (cddr map)))
    (or (plist-get plist :display-icon)
	(let* ((kind-face (plist-get plist :face))
	       (col (if kind-face
			(face-attribute kind-face :foreground nil t)
		      (if kind-icon-default-face
			  (face-attribute kind-icon-default-face :foreground nil t)
			(frame-parameter nil 'foreground-color))))
	       (kind-face-bg (and kind-face (face-attribute kind-face :background nil t)))
	       (default-bg (if kind-icon-default-face
			       (face-attribute kind-icon-default-face :background nil t)
			     (frame-parameter nil 'background-color)))
	       (bg-col (if kind-icon-blend-background
			   (kind-icon--rgb-blend
			    (color-name-to-rgb col) 
			    (color-name-to-rgb default-bg)
			    kind-icon-blend-frac)
			 (if (and kind-face-bg (not (eq kind-face-bg 'unspecified)))
			     kind-face-bg
			   default-bg)))
	       (dfw (default-font-width))
	       (half (/ dfw 2))
	       (pad-right (propertize " " 'display `(space :width (,half))))
	       (pad-left (propertize " " 'display `(space :width (,(- dfw half)))))
	       (disp (if-let ((kind-icon-use-icons)
			      (icon (plist-get plist :icon)))
			 ;; icon: always 2x1, half-space on each side
			 (propertize (concat
				      pad-left
				      (propertize "*" ; pretend 2 char-wide icon is only 1
						  'display (apply #'svg-lib-icon icon nil
								  :foreground col
								  :background bg-col
								  kind-icon-default-style))
				      pad-right) 'face `(:background ,bg-col))
		       ;; text, 1 or 2 chars, centered with full or half space on each side
		       (let* ((txt (truncate-string-to-width (cadr map) 2))
			      (len (length txt))
			      (txt-disp (if (eq len 2)
					    (concat pad-left (propertize "_" 'display txt) pad-right)
					  (concat " " txt " "))))
			 (propertize txt-disp 'face
				     `(:weight bold :foreground ,col :background ,bg-col))))))
	  (plist-put plist :display-icon disp)
	  disp))))

(defun kind-icon-reset-cache ()
  "Remove all cached icons from `kind-icon-mapping'."
  (interactive)
  (cl-loop for item in kind-icon-mapping
	   do (plist-put (cddr item) :display-icon nil)))

(defun kind-icon--affixation-function (kind-func &optional ann-func)
  "Create and return a custom kind-icon affixation function.
The company-kind function should be passed in as KIND-FUNC and
any annotation-function as ANN-FUNC.  The returned function
supplies a candiate kind badge -- abbreviated text key or icon --
as an affixation prefix.  ANN-FUNC, if non-nil, will be called
and its result used as the affixation suffix, first setting the
`completions-annotations' face on it."
  (lambda (candidates)
      (mapcar (lambda (cand)
		(let ((suffix (if ann-func (funcall ann-func cand) "")))
		  (add-face-text-property
		   0 (length suffix) 'completions-annotations 'append suffix)
		  (if-let ((kind (funcall kind-func cand))
			   (badge (kind-icon-formatted kind)))
		      (list cand badge suffix)
		    (list cand
			  (propertize "??" 'face '(:weight bold :background "red"))
			  suffix))))
	      candidates)))

(defun kind-icon-enhance-completion (completion-function)
    "A wrapper for completion-in-region-functions.
This wrapper sets a custom affixation-function which places an
icon in the prefix slot. Use it like:

  (setq completion-in-region-function 
     (kind-icon-enhance-completion 
       #'original-completion-in-region-function))"
    (lambda (start end table &optional pred)
      (let* ((str (buffer-substring start (point)))
	     (metadata (completion-metadata str table pred))
	     (kind-func (kind-icon--metdata-get metadata "company-kind"))
	     (ann-func (kind-icon--metdata-get metadata "annotation-function"))
	     (aff-func (kind-icon--metdata-get metadata "affixation-function")))
	(when (and kind-func (not aff-func)) ;; add a custom affixation function
	  (kind-icon-reset-cache)
	  (setq completion-extra-properties
		(plist-put completion-extra-properties :affixation-function
			   (kind-icon--affixation-function kind-func ann-func)))))
      (funcall completion-function start end table pred)))

(defun kind-icon--guard-config (_s _n _o where)
  "Dump the variable cache when the variable changes."
  (if where
      (with-current-buffer where
	(kind-icon-reset-cache))
    (kind-icon-reset-cache)))

(add-variable-watcher 'kind-icon-mapping #'kind-icon--guard-config)
(add-variable-watcher 'kind-icon-use-icons #'kind-icon--guard-config)
(add-variable-watcher 'kind-icon-blend-background #'kind-icon--guard-config)
(add-variable-watcher 'kind-icon-blend-frac #'kind-icon--guard-config)
(provide 'kind-icon)
