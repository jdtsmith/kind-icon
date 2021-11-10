;;; kind-icon.el --- Completion kind icons  -*- lexical-binding: t -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/kind-icon
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.0.1
;; Keywords: completion

;;; Commentary:

;; kind-icon-mode adds an icon or text prefix based on :company-kind
;; for compatible completion UI's utilizing completion-in-region.  The
;; "kind" prefix is typically used for differentiating variables,
;; functions, etc. among completion results.  It works by creating and
;; setting into `completion-extra-properties' a custom
;; affixation-function.  This function creates, styles, and caches a
;; short-text or icon-based "badge" representing the kind of the
;; candidate.  Icons are by default loaded from the "material" library
;; provided by svg-lib, which is required (unless only short-text
;; badges are desired, see `kind-icon-use-icons').

;; kind-icon is free software: you can redistribute it
;; and/or modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Python-MLS is distributed in the hope that it will be useful,
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
    (constant "c" :icon "lock-remove-outline" :face font-lock-constant-face)
    (enum-member "e" :icon "format-list-checks" :face font-lock-builtin-face)
    (enum "e" :icon "format-list-bulleted-square" :face font-lock-builtin-face)
    (field "f" :icon "application-braces-outline" :face font-lock-variable-name-face)
    (file "f" :face :icon "file-document-outline" font-lock-string-face)
    (folder "d" :icon "folder" :face font-lock-doc-face)
    (interface "i" :icon "application-brackets-outline" :face font-lock-type-face)
    (keyword "k" :icon "key-variant" :face font-lock-keyword-face)
    (method "m" :icon "function-variant" :face font-lock-function-name-face)
    (function "f" :icon "function" :face font-lock-function-name-face)
    (module "{" :icon "file-code-outline" :face font-lock-type-face)
    (numeric "n" :icon "numeric" :face font-lock-builtin-face)
    (operator "o" :icon "plus-minus" :face font-lock-comment-delimiter-face)
    (parameter "p" :icon "application-variable-outline" :face font-lock-builtin-face)
    (property "p" :icon "application-parentheses-outline" :face font-lock-variable-name-face)
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

   (KIND SHORT-TEXT LIBRARY ICON FACE-OR-COLOR)

This information is used to build a prefix for kind KIND.  A
prefix is a propertized string of either the-short TEXT or
ICON (from LIBRARY; see `svg-icon'), depending on the value of
variable `kind-icon-use-icons' . FACE-OR-COLOR can either be a
color string or a face from which we the :foreground
face-property is taken. The background is automatically computed
to lie between the background color and foreground (see
`kind-icon-blend-frac')."
  :type 'list)

(defcustom kind-icon-blend-frac 0.12
  "Fractional blend between foreground and background colors.
This is used for the background for the short-text kind
prefixes."
  :type 'float)

(defcustom kind-icon-default-face nil
  "The default face to use for coloring.
Normally foreground colors are supplied by the face matching in
`kind-icon-mapping', but if no face is supplied in the mapping,
the foreground color is taken from the foreground of this face,
or (if nil) to the default frame foreground color.  The background
color for blending the foreground into the background is also
taken from this face, if provided, defaulting to the frame
background color."
  :type 'face)

(defcustom kind-icon-default-style
  '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 1.0)
  "Default style parameters for building SVG icons.
See `svg-lib-style-compute-default'."
  :type 'plist)

(defsubst kind-icon--rgb-blend (rgb1 rgb2 frac)
  "Return a fractional blend between two colors RGB1 and RGB2.
Each is a 3 element list.  The fractional blend point is the
float FRAC."
  (apply #'color-rgb-to-hex
	 (cl-mapcar (lambda (a b)
		      (+ (* a frac) (* b (- 1.0 frac))))
		    rgb1 rgb2)))

(defconst kind-icon--unknown
  (propertize "??" 'face '(:weight bold :foreground "Red")))

(defsubst kind-icon--metdata-get (metadata type-name)
  (or
   (cdr (assq (intern type-name) metadata))
   (plist-get completion-extra-properties (intern (format ":%s" type-name)))))

(defun kind-icon-badge (kind)
  "Return a kind badge, either an SVG icon or short-text abbreviation.
Caches as :display-icon in `kind-icon-mapping', and returns the
cached value, if set.  For the background color, computes a blend
between a nominal background color (from either the frame
background color, or the :background property
`kind-icon-default-face', if set). See
`kind-icon-blend-frac'.  For the foreground color, uses the
:face mapping's :foreground color, the `kind-icon-default-face'
foreground, or the default frame foreground, in that order of
priority."
  (when-let ((map (assq kind kind-icon-mapping))
	     (plist (cddr map)))
    (or (plist-get plist :display-icon)
	(let* ((bg-rgb (color-name-to-rgb
			(if kind-icon-default-face
			    (face-attribute kind-icon-default-face :background nil t)
			  (frame-parameter nil 'background-color))))
	       (col-face (plist-get plist :face))
	       (col (if col-face
			(face-attribute col-face :foreground)
		      (if kind-icon-default-face
			  (face-attribute kind-icon-default-face :foreground nil t)
			(frame-parameter nil 'foreground-color))))
	       (bg-col (kind-icon--rgb-blend
			(color-name-to-rgb col) bg-rgb
			kind-icon-blend-frac))
	       (disp (if-let ((kind-icon-use-icons)
			      (icon (plist-get plist :icon)))
			 (propertize "**" 'face `(:background ,bg-col)
				     'display (apply #'svg-lib-icon icon nil
						     :foreground col :background bg-col
						     kind-icon-default-style))
		       (propertize
			(cadr map) 'face
			`(:weight bold :foreground ,col :background ,bg-col)))))
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
			   (badge (kind-icon-badge kind)))
		      (list cand badge suffix)
		    (list cand kind-icon--unknown suffix))))
	      candidates)))

(defvar-local kind-icon--orig-completion-function nil
  "The prior completion-in-region-function we are wrapping.")

(defun kind-icon--completion-in-region-function (start end table &optional pred)
  "Set a custom affixation function for kind-icon.
Only operates if no affixation function is already set."
  (let* ((str (buffer-substring start (point)))
	 (metadata (completion-metadata str table pred))
	 (kind-func (kind-icon--metdata-get metadata "company-kind"))
	 (ann-func (kind-icon--metdata-get metadata "annotation-function"))
	 (aff-func (kind-icon--metdata-get metadata "affixation-function")))
    (if (and kind-func (not aff-func)) ;; add a custom affixation function
	(setq completion-extra-properties
	      (plist-put completion-extra-properties :affixation-function
			 (kind-icon--affixation-function kind-func ann-func)))))
  (funcall kind-icon--orig-completion-function start end table pred))
		  
(define-minor-mode kind-icon-mode
  "Minor mode enabling kind prefix by wrapping the completion-in-region-function."
  :init-value nil
  (if completion-in-region-function
      (if kind-icon-mode
	  (progn
	    (kind-icon-reset-cache)
	    (setq-local
	     kind-icon--orig-completion-function completion-in-region-function
	     completion-in-region-function #'kind-icon--completion-in-region-function))
	(setq-local
	 completion-in-region-function kind-icon--orig-completion-function
	 kind-icon--orig-completion-function nil))
    (error "Cannot enable kind-icon: no completion-in-region-function found")))

(provide 'kind-icon)
