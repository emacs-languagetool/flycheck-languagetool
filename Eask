;; -*- mode: eask; lexical-binding: t -*-

(package "flycheck-languagetool"
         "0.4.2"
         "Flycheck support for LanguageTool")

(website-url "https://github.com/emacs-languagetool/flycheck-languagetool")
(keywords "convenience" "grammar" "check")

(package-file "flycheck-languagetool.el")

(script "test" "eask test ert ./test/*.el")

(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "flycheck")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
