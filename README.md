[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flycheck-languagetool-badge.svg)](https://melpa.org/#/flycheck-languagetool)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-languagetool-badge.svg)](https://stable.melpa.org/#/flycheck-languagetool)

<img align="right" src="./etc/logo.png" with="153" height="46">

# flycheck-languagetool
> Flycheck support for LanguageTool.

[![CI](https://github.com/emacs-languagetool/flycheck-languagetool/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-languagetool/flycheck-languagetool/actions/workflows/test.yml)

## :floppy_disk: Installation

The instruction to use this plugin.

1. Download LanguageTool from https://languagetool.org/download/.
2. Extract on to your local machine.
3. Consider adding the following snippet to your configuration.

```el
(use-package flycheck-languagetool
  :ensure t
  :hook (text-mode . flycheck-languagetool-setup)
  :init
  (setq flycheck-languagetool-server-jar "path/to/LanguageTool-X.X/languagetool-server.jar"))
```

4. :tada: Done! Now open a text file and hit `M-x flycheck-mode`!

## :wrench: Configuration

* `flycheck-languagetool-url`
* `flycheck-languagetool-server-jar`
* `flycheck-languagetool-server-port`
* `flycheck-languagetool-active-modes`
* `flycheck-languagetool-language` (Default `"en-US"`)

### :book: Spellchecking

LanguageTool’s spellchecking will be disabled if `flyspell-mode` is
active.  Disable `flyspell-mode` if you would prefer LanguageTool to
check for spelling mistakes.

## Troubleshooting

You can find log messages from a local LanguageTool server in a hidden buffer called ` *LanguageTool server*`.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
