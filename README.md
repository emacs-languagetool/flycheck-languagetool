[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![MELPA](https://melpa.org/packages/flycheck-languagetool-badge.svg)](https://melpa.org/#/flycheck-languagetool)
[![MELPA Stable](https://stable.melpa.org/packages/flycheck-languagetool-badge.svg)](https://stable.melpa.org/#/flycheck-languagetool)

<img align="right" src="./etc/logo.png" with="153" height="46">

# flycheck-languagetool
> Flycheck support for LanguageTool.

[![CI](https://github.com/emacs-languagetool/flycheck-languagetool/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-languagetool/flycheck-languagetool/actions/workflows/test.yml)

## 💾 Installation

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

## 🔧 Configuration

* `flycheck-languagetool-url`
* `flycheck-languagetool-server-jar`
* `flycheck-languagetool-server-port`
* `flycheck-languagetool-active-modes`
* `flycheck-languagetool-language` (Default `"en-US"`)

### 📖 Spellchecking

LanguageTool’s spellchecking will be disabled if
[`flyspell-mode`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html)
or [`jinx-mode`](https://github.com/minad/jinx) is active. Disable those
modes if you would prefer LanguageTool to check for spelling mistakes.

## ❓ Troubleshooting

You can find log messages from a local LanguageTool server in a hidden buffer called ` *LanguageTool server*`.

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
$ eask install-deps --dev
```

To test the package's installation:

```sh
$ eask package
$ eask install
```

To test compilation:

```sh
$ eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
$ eask lint checkdoc
```

The standard `package` linter:

```sh
$ eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
