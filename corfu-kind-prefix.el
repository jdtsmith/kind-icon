;;; corfu-kind-prefix.el --- Kind prefixes in corfu  -*- lexical-binding: t -*-

;; Copyright (C) 2021  J.D. Smith

;; Author: J.D. Smith
;; Homepage: https://github.com/jdtsmith/corfu-kind-prefix
;; Package-Requires: ((emacs "27.1"))
;; Package-Version: 0.0.1
;; Keywords: completion

;;; Commentary:

;; This package adds a prefix based on :company-kind for corfu,
;; e.g. for differentiating variables, functions, etc. in completion.
;; It works by adding a custom affixation-function, if none yet
;; exists.

;; company-kind-prefix is free software: you can redistribute it
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

(require 'corfu)

(defgroup corfu-kind nil
  "Corfu prefixes from :company-kind."
  :group 'convenience
  :prefix "corfu-kind")

(defcustom corfu-kind-mapping ;; adapted from company
  '((array "a" font-lock-type-face)
    (boolean "b" font-lock-builtin-face)
    (class "c" font-lock-type-face)
    (color "#" success)
    (constant "c" font-lock-constant-face)
    (enum-member "e" font-lock-builtin-face)
    (enum "e" font-lock-builtin-face)
    (field "f" font-lock-variable-name-face)
    (file "f" font-lock-string-face)
    (folder "d" font-lock-doc-face)
    (interface "i" font-lock-type-face)
    (keyword "k" font-lock-keyword-face)
    (method "m" font-lock-function-name-face)
    (function "f" font-lock-function-name-face)
    (module "{" font-lock-type-face)
    (numeric "n" font-lock-builtin-face)
    (operator "o" font-lock-comment-delimiter-face)
    (parameter "p" font-lock-builtin-face)
    (property "p" font-lock-variable-name-face)
    (ruler "r" shadow)
    (snippet "S" font-lock-string-face)
    (string "s" font-lock-string-face)
    (struct "%" font-lock-variable-name-face)
    (text "tx" shadow)
    (value "v" font-lock-builtin-face)
    (variable "va" font-lock-variable-name-face)
    (t "." shadow))
  "Mapping of the text kinds.
The format should be an alist of (KIND text FG) which is used to
propertize the short TEXT to be shown for a candidate of kind
KIND. FG can either be a color string or a face from which we can
get a color string (using the :foreground face-property). The
background is automatically computed to lie between the corfu
background color and FG (see `corfu-kind-blend-frac')."
  :type 'list)

(defcustom corfu-kind-blend-frac 0.12
  "Fractional blend between foreground and background colors.
This is used for the background for the short-text kind
prefixes."
  :type 'float)

(defsubst corfu-kind--rgb-blend (rgb1 rgb2 frac)
  "Return a fractional blend between two colors RGB1 and RGB2.
Each is a 3 element list.  The fractional blend point is the
float FRAC."
  (apply #'color-rgb-to-hex
	 (cl-mapcar (lambda (a b)
		      (+ (* a frac) (* b (- 1.0 frac))))
		    rgb1 rgb2)))

(defun corfu-kind--affixation-function (candidates)
  "Supply an abbreviated form of the candidate kind as an affixation prefix.
Only operates if an affixation-function is not already
present. Candidate kind is taken from `:company-kind'. Applies a
foreground color based on matching font-lock faces for the
candidate type, and a background mixed between this color and the
`corfu-background' face's background color."
  (let ((kind-func (plist-get corfu--extra :company-kind))
	(ann (or (corfu--metadata-get corfu--metadata 'annotation-function)
                 (plist-get corfu--extra :annotation-function)))
	(bg-rgb (color-name-to-rgb
		 (face-attribute 'corfu-background :background))))

    (mapcar (lambda (cand)
              (if-let ((kind (funcall kind-func cand))
		       (suffix (if ann (funcall ann cand) ""))
		       (mapping (assq kind corfu-kind-mapping))
		       (prefix (cadr mapping))
		       (col (face-attribute (caddr mapping) :foreground))
		       (bg-col (corfu-kind--rgb-blend (color-name-to-rgb col) bg-rgb
						      corfu-kind-blend-frac)))
		  (list cand
			(propertize prefix 'face
				    `(:weight bold :foreground ,col :background ,bg-col))
			suffix)
		(list cand "u" suffix)))
            candidates)))
    
(defun corfu-kind--setup ()
  (if (and completion-in-region-mode
	   (plist-get corfu--extra :company-kind)
	   (not (or (corfu--metadata-get corfu--metadata 'affixation-function)
		    (plist-get corfu--extra :affixation-function))))
      (plist-put corfu--extra :affixation-function
		 #'corfu-kind--affixation-function)))

;;;###autoload
(advice-add #'corfu--setup :after #'corfu-kind--setup)
