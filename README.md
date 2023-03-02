# kind-icon — colorful icons for completion in Emacs
This emacs package adds configurable icon or text-based completion prefixes based on the `:company-kind` property that many completion backends (such as lsp-mode and Emacs 28's elisp-mode) provide.

## Examples

A few examples of `kind-icon` in action with [lsp-mode](https://github.com/emacs-lsp/lsp-mode) providing completions:

<div align="center">

|   |   |
| :---: | :---: |
| **With [corfu](https://github.com/minad/corfu) and [orderless](https://github.com/oantolin/orderless):**  |   **Using text-based icons:** |
| ![image](https://user-images.githubusercontent.com/93749/141227979-9f22cbb6-8029-42f1-86b4-f4cdd03636b2.png) | ![image](https://user-images.githubusercontent.com/93749/141225205-9a718be8-f352-451f-892b-aaacae1eeaf6.png) |
| **Mix of text and icons:**  |   **Blended background color disabled:** |
| ![image](https://user-images.githubusercontent.com/93749/141231327-5b15a92f-87f6-4a52-aff4-d7e9229842a9.png) | ![image](https://user-images.githubusercontent.com/93749/141227004-e4514961-245c-4aa0-888a-65c0a1b63757.png) |
</div>

## Installation 

Get it from ELPA (e.g. `M-x package-install RET kind-icon RET`).  Note that icon support requires the small library [svg-lib](https://github.com/rougier/svg-lib).  At present `kind-icon` has been tested extensively with the excellent [corfu](https://github.com/minad/corfu) in-buffer completion front-end (from the maker of vertico, consult, marginalia, and more). 

`kind-icon` works either as a "margin-formatter" (for supporting UI's such as corfu) or by wrapping the completion function, for other completion UI's which can handle the Emacs 28+ [`affixation-function`](https://git.savannah.gnu.org/cgit/emacs.git/tree/doc/lispref/minibuf.texi?id=d8e037eeaa7eef26349bc0fb3fa00e10a5c4b894#n1819) completion property.  

### Using margin-formatters (preferred):

To enable for completion UI's with margin-formatters capability such as [corfu](https://github.com/minad/corfu):

```elisp
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
```

### Wrapping completion-in-region:

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

for whichever `completion-ui` you are using.  Note that for this method to work, your completion UI must support the Emacs 28 `affixation-function` property. 

## Configuration

The configuration defaults should work fine, but `kind-icon` can be customized to change the icons and colors, preference of icons vs. short-text (or mixed) prefixes, and more. 

### Variables

`kind-icon` has a few customization variables that allow you to configure its appearance.  The easiest way to edit them is `M-x customize-group kind-icon`, which automatically takes care of cleaning the cache upon changes.  If you change them directly from lisp during a session (e.g. with `setq`), call `M-x kind-icon-reset-cache` to reset the temporary `kind-icon` cache, so that the changes will take effect.

#### Important configuration variables:

- `kind-icon-use-icons`: If non-nil (the default), prefer icons for prefix badges.  Otherwise, use text labels.  Individual kind entries can also have their icons disabled by removing the `:icon` property in the mapping (see below).  If `svg-lib` is unable to download a named icon and it is not cached on disk, text labels for that kind will be used as a fallback. 

- `kind-icon-mapping`: This is the top level configuration mapping `:company-kind` "types" like `'variable` and `'function`.  Each item in this list has the format `(sym short-text :keyword value ...)` where `sym` is the kind (a symbol), and `short-text` is the abbreviated text to display (if icons are not used), both of which must be present.  The rest of the list is a property list with optional keys `:icon` and `:face`.  The latter will be used to set the text foreground and (possibly) background colors on the badge.  The former specifies an icon to use, and is simply a string name from the free [material icon library](https://pictogrammers.com/library/mdi/) used by [svg-lib](https://github.com/rougier/svg-lib).  Try `M-x customize-variable kind-icon-mapping` for a nice interface including an icon Preview button.  Pro-tip: keep your code buffer open with completion popped-up alongside the customization buffer.  When you apply changes, they are immediately applied.

- `kind-icon-default-face`: A face from which the icon background color will be taken and blended with the `:face` foreground color in the mapping table to create a custom background color.  If not set, the frame default background color will be used for this purpose.  Similarly, the foreground color for this face, if set, will be used if a `:face` foreground is missing from the mapping. 

- `kind-icon-blend-background`: If non-nil, computes a blend between a nominal background color (from either the background property of `kind-icon-default-face`, if set, or frame background color) and the foreground :face.  If `kind-icon-blend-background` is nil, the background is taken from the :face background, `kind-icon-default-face`, or frame background-color.

- `kind-icon-blend-frac`: The fractional blend between custom badge `:face` foreground and background (see above) color to use as a custom background for each badge.  A value of 0.0 simply replicates the background color.  Values should likely stay below 0.3 or so.

### Colors

If you don't like the default colors of the icons, you can customize the associated face, choose another pre-existing face, or substitute your own face. You can also change how the background color is displayed. 

#### Foreground color

Icon foreground colors are matched in the default mapping to the face colors used by font-lock in programming modes (variables, function names, etc.).  This gives consistency with in-buffer highlighting.  These colors are taken from the `:face` `:foreground` color in `kind-icon-mapping`.  If no `:face` is set for some kind, the foreground is taken from `kind-icon-default-face` foreground, or, as a backup, the default frame foreground.

#### Background color

By default, `kind-icon` creates a _blended_ background color that is a mix of a bit of the foreground color and the normal completion background (control the mix with `kind-icon-blend-frac`).  Note that if your completion UI uses a different background color from your normal buffer, you should configure the face it uses in `kind-icon-default-face`. If you disable `kind-icon-blend-background`, `kind-icon` will use both the foreground _and_ (if set) background from the configured `:face` for each kind, allowing you to configure arbitrary colors.

### Icons 

Check the [material icon library](https://pictogrammers.com/library/mdi/) for the icons you can use, more than 7,000 of them!  All you need to "use" an icon is its name.  The easiest approach is to `M-x customize-variable kind-icon-mapping`, find the kind you are interested in, and change its icon. Hit the `Preview` button and check the message buffer to confirm it's the icon you were after, and Apply your changes.

**Note that `svg-lib`, which `kind-icon` uses, downloads and caches icons, by default in `.emacs.d/.cache/svg-lib/`.**  If no network connection is present, and the icon has not been cached on disk, the short-text is used as a backup for that session. 

And yes, you can use **any icons**!

<div align="center">

  ![image](https://user-images.githubusercontent.com/93749/141231207-94d14bd8-0e85-4315-aa29-f6200b2729cc.png)

</div>

### Old School: Text-based Icons

You can also use simple text-based prefixes instead of icons.  The icons are quite lightweight so there shouldn't be much performance difference, but some may prefer a simpler look.  A "text" icon is composed of either one or two characters (anything longer will be trimmed).  Simply set the `kind-icon-use-icons` variable to `nil` and (if desired) customize the "Short-Text" in the mapping.  Note that if you are not connected to the network, even if you have enabled icons, any icons which are not cached on disk will be replaced by their short text equivalents.

### Previewing

Use `M-x kind-icon-preview-all` to reset and preview all icons (text and SVG icons both) in a view buffer.  This also has the effect of pre-downloading all icons, courtesy svg-lib. Current defaults:

<div align="center">

  ![image](https://user-images.githubusercontent.com/93749/167690072-d8822ada-62b8-4b38-b8ea-987d83b38951.png)

</div>

### Debugging Tips

If you get an error mentioning `corfu--post-command`, and notice that you don't get a backtrace even after invoking `toggle-debug-on-error`, this is because backtraces are inhibited during post-command hooks.  To re-enable them, evaluate the following (e.g. in your `*scratch*` buffer):


```elisp
(advice-add 'corfu--post-command :around
	    (lambda (func)
	      (condition-case err
		  (funcall func)
		((debug error) (signal (car err) (cdr err))))))
```

## Related Packages

A small set of packages related to `kind-icon`:

- [corfu](https://github.com/minad/corfu) is a sleek in-buffer completion frontend which fully supports standard emacs completions providers (aka completion at point functions — CAPFs).  It was the original to include the margin-formatter function support which kind-icon uses.
- [company-mode](https://github.com/company-mode/company-mode) is the all-inclusive completion system, and includes built-in support for icons based on the Microsoft VSCode set.
- [all-the-icons](https://github.com/iyefrat/all-the-icons.el) is font-based icon library which can be used to enrich dired, neotree, the mode line and others with icons.
- [all-the-icons-completion](https://github.com/iyefrat/all-the-icons-completion/) builds on `all-the-icons` to enrich minibuffer-based completion, including support for the excellent [marginalia](https://github.com/minad/marginalia) annotator. 

## Notes

If you are using the emacs-mac fork of emacs on MacOS >=10.13, you should compile with `librsvg` support, as the native SVG support using WebKit2 is slow and will impact performance. 

## Thanks

- to @rougier for the excellent [svg-lib](https://github.com/rougier/svg-lib).
- to @minad, who developed [corfu](https://github.com/minad/corfu) (among many others) and contributed many great ideas to `kind-icon`.
- to @dgutov, maintainer of [company](https://github.com/company-mode/company-mode), from which color-matching to font-lock faces was inspired. 
