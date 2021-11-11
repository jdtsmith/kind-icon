# kind-icon — Rich icons for in-buffer completion
This emacs package adds icon or text-based completion prefixes based on the `:company-kind` property many completion backends (such as lsp-mode) provide.  It works either as a "kind-formatter" function (e.g. for corfu) or by wrapping the completion function for other completion UI's which handle the Emacs 28+ `affixation-function` completion property. 

## Examples

- **`kind-icon` with [corfu](https://github.com/minad/corfu) and [orderless](https://github.com/oantolin/orderless), from [lsp-mode](https://github.com/emacs-lsp/lsp-mode) :**

  ![image](https://user-images.githubusercontent.com/93749/141227979-9f22cbb6-8029-42f1-86b4-f4cdd03636b2.png)
  
- **With text-based icons:**

  ![image](https://user-images.githubusercontent.com/93749/141225205-9a718be8-f352-451f-892b-aaacae1eeaf6.png)

- **Blended background color disabled:**

  ![image](https://user-images.githubusercontent.com/93749/141227004-e4514961-245c-4aa0-888a-65c0a1b63757.png)

- **Mix of text and icons:** 

  ![image](https://user-images.githubusercontent.com/93749/141228094-8f3364b8-09d0-4e17-860d-221ea9de989c.png)

## Installation 

Get it from melpa (TBD), which should install `svg-lib` as well if necessary.  Enable in any buffer with completion using `kind-icon-mode`.  E.g., to enable for the completion UI [corfu](https://github.com/minad/corfu), which has a kind-formatter configuration:

```elisp
(use-package kind-icon ;package availability TBD
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-background)
  (corfu-kind-formatter #'kind-icon-formatted))
```

The more generic approach of wrapping the `completion-in-region-function` would look like:

```elisp
(use-package kind-icon
  :ensure t
  :config
  (add-hook 'my-completion-ui-mode-hook
   	    (lambda ()
   	      (setq completion-in-region-function
   		    (kind-icon-enhance-completion
   		     completion-in-region-function)))))
```

## Configuration

The configuration defaults should work fine, but kind-icon can be customized to change the colors, preference of icons vs. short-text (or mixed) prefixes, and more. 

### Variables

`kind-icon` has a few customization variables that allows you to configure its appearance.

**Key configurations:**

- `kind-icon-use-icons`: If non-nil (the default), prefer icons for prefix badges.  Otherwise, use text labels.  Individual kind entries can also have their icons disabled by removing the `:icon` property in the mapping (see below). 

- `kind-icon-mapping`: This is the top level configuration mapping `:company-kind` "types" like `'variable` and `'function`.  Each item in this list has the format `(sym short-text :keyword value ...)` where `sym` is the kind (a symbol), and `short-text` is the abbreviated text to display (if icons are not used).  The rest of the list is a property list with optional keys `:icon` and `:face`.  The latter will be used to set the text foreground and (possibly) background colors on the badge.  The former specifies an icon to use, and is simply a string name from the free [material icon library](https://materialdesignicons.com) used by [svg-lib](https://github.com/rougier/svg-lib).  Try `M-x customize-variable kind-icon-mapping` for a nice interface including an icon Preview button.

- `kind-icon-default-face`: A face from which the icon background color will be taken and blended with the `:face` foreground color in the mapping table to create a custom background color.  If not set, the frame default background color will be used for this purpose.  Similarly, the foreground color for this face, if set, will be used if a `:face` foreground is missing from the mapping. 

- `kind-icon-blend-background`: If non-nil, computes a blend between a nominal background color (from either the background property of `kind-icon-default-face`, if set, or frame background color) and the foreground :face.  If `kind-icon-blend-background` is nil, the background is taken from the :face background, `kind-icon-default-face`, or frame background-color.

- `kind-icon-blend-frac`: The fractional blend between custom badge `:face` foreground and background (see above) color to use as a custom background for each badge.  A value of 0.0 simply replicates the background color.  Values should likely stay below 0.3 or so.

### Colors

If you don't like the default colors used to go along with the icons, you can customize the associated face, choose another, or build your own. You can also change how the background color is displayed. 

**Foreground color:**

Icon foreground colors are matched by default to the default colors in programming modes (variables, function names, etc.).  This gives consistency with in-buffer highlighting.  These colors are taken from the `:face` mapping's `:foreground` color.  If no `:face` is set, the foreground is taken from `kind-icon-default-face` foreground, or, as a backup the default frame foreground.

**Background color:**

By default, `kind-icon` creates a blended background color that is a mix of a bit of the foreground color and the background.  Note that if your completion UI uses a different background color from your normal buffer, you should configure the face it uses in `kind-icon-default-face`. If you turn off `kind-icon-blend-background`, `kind-icon` will use both the foreground _and_ background from the configured `:face` for each kind, allowing you to configure arbitrary colors.

### Icons 

Check the [material icon library](https://materialdesignicons.com) for the SVG icons you can use.  All you need to "use" an icon is its name! Note that on first use, icons are auto-downloaded by `svg-lib`, so you'll need a network connection.  After that they are cached locally. 
