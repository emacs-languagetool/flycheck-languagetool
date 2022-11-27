# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## Unreleased

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security

## [0.4.0] - 2022-11-27

### Added

- Allow LanguageTool command to be configured via customisable variable `flycheck-languagetool-server-command`.
- Add a change log.

### Changed

- Make some warnings and error messages more verbose.
- Broaden automated testing to more versions of Emacs.
- Replace Cask with Eask.

### Fixed

- Support narrowed buffers.

## [0.3.0] - 2021-11-20

- **Incompatible change**: For better performance, a local LanguageTool server is now started once instead of repeatedly starting up the LanguageTool command-line tool for each check.  See `flycheck-languagetool-server-jar` to set the path to the server JAR.
- Reduce CPU usage.
- Don’t try to enable this checker if LanguageTool is missing or not configured.
- If the checker couldn’t be enabled, explain why with `flycheck-verify-checker`.
- This checker is now activated for `message-mode` buffers.
- Link each reported warning to a [detailed explanation on the LanguageTool website](https://community.languagetool.org/rule/list).
- Disable LanguageTool’s spell checker if `flyspell` is in use.
- Chain to Flycheck’s built-in [Proselint](http://proselint.com/) checker.

## [0.2.0] - 2021-06-19

- Allow checker to check when `buffer-file-name` is `nil`, see [#5](https://github.com/emacs-languagetool/flycheck-languagetool/issues/5)
- Fix error when buffer is killed before checking

## [0.1.0] - 2021-04-05

Initial release.
