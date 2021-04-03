;;; flycheck-languagetool.el --- Flycheck support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Flycheck support for LanguageTool.
;; Keyword: grammar check
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (flycheck "0.14"))
;; URL: https://github.com/emacs-languagetool/flycheck-languagetool

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flycheck support for LanguageTool.
;;

;;; Code:

(require 'flycheck)

(defgroup flycheck-languagetool nil
  "Flycheck support for LanguageTool."
  :prefix "flycheck-languagetool-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/flycheck-languagetool"))

(defconst flycheck-languagetool--home-dir (file-name-directory load-file-name)
  "`flycheck-languagetool' home directory.")

(defcustom flycheck-languagetool-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with LanguageTool."
  :type 'list
  :group 'flycheck-languagetool)

(flycheck-def-option-var flycheck-languagetool-commandline-jar
    (expand-file-name "lib/languagetool-commandline.jar" flycheck-languagetool--home-dir)
    languagetool
  "The path of languagetool-commandline.jar."
  :type '(file :must-match t))

(flycheck-def-option-var flycheck-languagetool-language "en-US" languagetool
  "The language code of the text to check."
  :type '(string :tag "Language")
  :safe #'stringp)
(make-variable-buffer-local 'flycheck-languagetool-language)

(defun flycheck-languagetool--parser (output checker buffer)
  "Parse error by OUTPUT, CHECKER, BUFFER."
  (mapcar
   (lambda (match)
     (let-alist match
       (flycheck-error-new-at
        (line-number-at-pos (1+ .offset))
        (save-excursion
          (goto-char (1+ .offset))
          ;; Flycheck 1-base, Emacs 0-base
          (1+ (current-column)))
        'warning
        .message
        :id .rule.id
        :checker checker
        :buffer buffer
        :filename (buffer-file-name buffer))))
   (alist-get 'matches (car (flycheck-parse-json output)))))

(flycheck-define-checker languagetool
  "Style and grammar checker using LanguageTool."
  :command ("java"
            (option "-jar" flycheck-languagetool-commandline-jar)
            ;;(option "-l" flycheck-languagetool-language)
            "-adl"
            "--json"
            "-")
  :standard-input t
  :error-parser flycheck-languagetool--parser
  :modes (text-mode)
  :predicate
  (lambda ()
    (and flycheck-languagetool-commandline-jar
         (file-exists-p flycheck-languagetool-commandline-jar)))
  :verify
  (lambda (_)
    (let ((have-jar
           (and flycheck-languagetool-commandline-jar
                (file-exists-p flycheck-languagetool-commandline-jar))))
      (list
       (flycheck-verification-result-new
        :label (or flycheck-languagetool-commandline-jar
                   "languagetool-commandline.jar")
        :message (if have-jar "exist" "doesn't exist")
        :face (if have-jar 'success '(bold error)))))))

;;;###autoload
(defun flycheck-languagetool-setup ()
  "Setup Flycheck LanguageTool."
  (add-to-list 'flycheck-checkers 'languagetool))

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
