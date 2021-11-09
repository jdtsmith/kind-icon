# kind-prefix
_Kind_ icon or short-text prefix badge labels for emacs in-region completion. 

This emacs package adds icon or text-based completion prefixes based on the `:company-kind` property many completion backends (such as lsp-mode) provide.  It works by creating a custom `affixation-function` for in-buffer completion, if the backend does not already provide one.  An affixation function specifies a prefix and suffix to go along with completion candidate text.

## Installation 

Get it from melpa (TBD).  Enable in any buffer with completion enabled using `kind-prefix-mode`.  E.g., to enable for the completion UI [corfu](https://github.com/minad/corfu):

```elisp
(use-package kind-prefix ;package availability TBD
  :ensure t
  :hook (corfu-mode . kind-prefix-mode))
```

## Configuration

Defaults should normally work fine, but some of the important configuration variables include:

- `kind-prefix-use-icons`: If non-nil (the default), prefer icons for prefix badges.

- `kind-prefix-mapping`: This is the top level configuration mapping `:company-kind` "types" like `'variable` and `'function`.  Each item in this list has the format `(sym short-text :keyword value ...)` where `sym` is the kind (a symbol), and `short-text` is the abbreviated text to display (if icons are not used).  The rest of the list is a property list with optional keys `:icon` and `:face`.  The latter will be used to set the text foreground and background colors on the badge.  The former specifies an icon to use, and is a string name from the free [material icon library](https://materialdesignicons.com) used by [svg-lib](https://github.com/rougier/svg-lib).  Note that since the icons are cached by `kind-prefix`, changes to this mapping or font size should be followed by `M-x kind-prefix-reset-cache` (or just restart Emacs).

- `kind-prefix-default-face`: A face from which background color will be taken and blended with the `:face` foreground color in the mapping table to create a custom background color.  If not set, the frame default background color will be used for this purpose.  Similarly, the foreground color for this face, if set, will be used if a `:face` foreground is missing from the mapping. 

- `kind-prefix-blend-frac`: The fractional blend between custom badge
`:face` foreground and background (see above) color to use as a custom
background for each badge.  A value of 0.0 simply replicates the
background color.  Values should likely stay below 0.3 or so.
