;;; flycheck-languagetool.el --- Flycheck support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  Shen, Jen-Chieh; Peter Oliver
;; Created date 2021-04-02 23:22:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;;         Peter Oliver <git@mavit.org.uk>
;; URL: https://github.com/emacs-languagetool/flycheck-languagetool
;; Version: 0.5.0
;; Package-Requires: ((emacs "27.1") (flycheck "39.0.0.20260813"))
;; Keywords: convenience grammar check

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

(require 'diff-mode)
(require 'flycheck)
(require 'json)
(eval-when-compile (require 'subr-x))

(defgroup flycheck-languagetool nil
  "Flycheck support for LanguageTool."
  :prefix "flycheck-languagetool-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/emacs-languagetool/flycheck-languagetool"))

(defcustom flycheck-languagetool-active-modes
  '(text-mode latex-mode org-mode markdown-mode markdown-ts-mode message-mode)
  "List of major mode that work with LanguageTool."
  :type 'list
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-url nil
  "The URL for the LanguageTool API we should connect to."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "URL"))
  :package-version '(flycheck-languagetool . "0.3.0")
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-command ()
  "Custom command to start LanguageTool server.
If non-nil, this list of strings replaces the standard java cli command."
  :type '(repeat string)
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-jar nil
  "The path of languagetool-server.jar.

The server will be automatically started if specified.  Set to
nil if you’re going to connect to a remote LanguageTool server,
or plan to start a local server some other way."
  :type '(choice (const :tag "Off" nil)
                 (file :tag "Filename" :must-match t))
  :package-version '(flycheck-languagetool . "0.3.0")
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
                   "https://dev.languagetool.org/http-server.html")
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-port 8081
  "The port on which an automatically started LanguageTool server should listen."
  :type 'integer
  :package-version '(flycheck-languagetool . "0.3.0")
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
                   "https://dev.languagetool.org/http-server.html")
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-args ()
  "Extra arguments to pass when starting the LanguageTool server."
  :type '(repeat string)
  :link '(url-link :tag "LanguageTool embedded HTTP Server"
                   "https://dev.languagetool.org/http-server.html")
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-language "en-US"
  "The language code of the text to check."
  :type '(string :tag "Language")
  :safe #'stringp
  :group 'flycheck-languagetool)
(make-variable-buffer-local 'flycheck-languagetool-language)

(defcustom flycheck-languagetool-check-params ()
  "Extra parameters to pass with LanguageTool check requests."
  :type '(alist :key-type string :value-type string)
  :options '("level"
             "enabledOnly"
             "disabledCategories"
             "enabledCategories"
             "disabledRules"
             "enabledRules"
             "preferredVariants"
             "motherTongue"
             "dicts"
             "apiKey"
             "username")
  :link '(url-link
          :tag "LanguageTool API"
          "https://languagetool.org/http-api/swagger-ui/#!/default/post_check")
  :group 'flycheck-languagetool)

(defvar flycheck-languagetool--started-server nil
  "Have we ever attempted to start the LanguageTool server?")

(defvar flycheck-languagetool--spelling-rules
  '("HUNSPELL_RULE"
    "HUNSPELL_RULE_AR"
    "MORFOLOGIK_RULE_AST"
    "MORFOLOGIK_RULE_BE_BY"
    "MORFOLOGIK_RULE_BR_FR"
    "MORFOLOGIK_RULE_CA_ES"
    "MORFOLOGIK_RULE_DE_DE"
    "MORFOLOGIK_RULE_EL_GR"
    "MORFOLOGIK_RULE_EN"
    "MORFOLOGIK_RULE_EN_AU"
    "MORFOLOGIK_RULE_EN_CA"
    "MORFOLOGIK_RULE_EN_GB"
    "MORFOLOGIK_RULE_EN_NZ"
    "MORFOLOGIK_RULE_EN_US"
    "MORFOLOGIK_RULE_EN_ZA"
    "MORFOLOGIK_RULE_ES"
    "MORFOLOGIK_RULE_GA_IE"
    "MORFOLOGIK_RULE_IT_IT"
    "MORFOLOGIK_RULE_LT_LT"
    "MORFOLOGIK_RULE_ML_IN"
    "MORFOLOGIK_RULE_NL_NL"
    "MORFOLOGIK_RULE_PL_PL"
    "MORFOLOGIK_RULE_RO_RO"
    "MORFOLOGIK_RULE_RU_RU"
    "MORFOLOGIK_RULE_RU_RU_YO"
    "MORFOLOGIK_RULE_SK_SK"
    "MORFOLOGIK_RULE_SL_SI"
    "MORFOLOGIK_RULE_SR_EKAVIAN"
    "MORFOLOGIK_RULE_SR_JEKAVIAN"
    "MORFOLOGIK_RULE_TL"
    "MORFOLOGIK_RULE_UK_UA"
    "SYMSPELL_RULE")
  "LanguageTool rules for checking of spelling.
These rules will be disabled if Emacs’ `flyspell-mode' or
`jinx-mode' is active.")

(defface flycheck-languagetool-suggestion-face
  '((t (:inherit diff-changed)))
  "Flycheck face for LanguageTool suggestions."
  :package-version '(flycheck-languagetool . "0.5.0")
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-suggestion-limit 12
  "The maximum number of correction suggestions to show per warning.
Any suggestions beyond this count will be ignored."
  :type '(integer :tag "Count")
  :safe (lambda (n)
          (and (integerp n)
               (< n 256))) ;; This number is somewhat picked out of the
                           ;; air, but large values can hurt
                           ;; performance.
  :package-version '(flycheck-languagetool . "0.5.0")
  :group 'flycheck-languagetool)

;;
;; (@* "External" )
;;

(defvar url-http-end-of-headers)
(defvar url-request-method)
(defvar url-request-extra-headers)
(defvar url-request-data)

;;
;; (@* "Core" )
;;

(defun flycheck-languagetool--check-all (results tick)
  "Map RESULTS from LanguageTool to positions of errors in the buffer.
TICK was the result of `buffer-chars-modified-tick' at the time of the check."
  (let ((matches (cdr (assoc 'matches results)))
        check-list)
    (dolist (match matches)
      (let* ((pt-beg (+ (point-min) (cdr (assoc 'offset match))))
             (len (cdr (assoc 'length match)))
             (pt-end (+ pt-beg len))
             (type 'warning)
             (id (cdr (assoc 'id (assoc 'rule match))))
             (subid (cdr (assoc 'subId (assoc 'rule match))))
             (replacements (cdr (assoc 'replacements match)))
             (fix (when replacements
                    (flycheck-fix-new
                     :description (cdr (assoc 'shortMessage match))
                     :edits (list
                             (flycheck-fix-edit-new-at-pos
                              pt-beg pt-end
                              (cdr (assoc 'value (car replacements)))))
                     :tick tick)))
             (desc
              (apply #'concat
                     (cdr (assoc 'message match))
                     (when replacements
                       (list
                        " Suggestions: "
                        (mapconcat
                         (lambda (replacement)
                           (let ((suggestion
                                  (copy-sequence
                                   (cdr (assoc 'value replacement)))))
                             (put-text-property
                              0
                              (length suggestion)
                              'face
                              'flycheck-languagetool-suggestion-face
                              suggestion)
                             suggestion))
                         (seq-take replacements
                                   flycheck-languagetool-suggestion-limit)
                         ", ")
                        (if (> (length replacements)
                               flycheck-languagetool-suggestion-limit)
                            "…"
                          "."))))))
        (push (list pt-beg type desc
                    :end-pos pt-end
                    :id (cons id subid)
                    :fix fix)
              check-list)))
    check-list))

(defun flycheck-languagetool--read-results (status source-buffer tick callback)
  "Callback for results from LanguageTool API.

STATUS is passed from `url-retrieve'.
SOURCE-BUFFER is the buffer currently being checked.
TICK was the result of `buffer-chars-modified-tick' at the time of the request.
CALLBACK is passed from Flycheck."
  (let ((err (plist-get status :error)))
    (when err
      (error
       (funcall callback 'errored
                (error-message-string
                 (append err
                         (list (progn
                                 (goto-char (+ 1 url-http-end-of-headers))
                                 (buffer-substring (point) (point-max))))))))))

  (if (buffer-live-p source-buffer)
      (progn
        (set-buffer-multibyte t)
        (goto-char url-http-end-of-headers)
        (let ((results (car (flycheck-parse-json
                             (buffer-substring (point) (point-max))))))
          (kill-buffer)
          (with-current-buffer source-buffer
            (funcall
             callback 'finished
             (mapcar
              (lambda (x)
                (apply #'flycheck-error-new-at-pos `(,@x :checker languagetool)))
              (condition-case err
                  (flycheck-languagetool--check-all results tick)
                (error (funcall callback 'errored (error-message-string err)))))))))
    (kill-buffer)
    (funcall callback 'interrupted nil)))

(defun flycheck-languagetool--start-server ()
  "Start the LanguageTool server if we didn’t already."
  (unless (process-live-p (get-process "languagetool-server"))
    (let* ((cmd (or flycheck-languagetool-server-command
                    (list "java" "-cp" (expand-file-name flycheck-languagetool-server-jar)
                          "org.languagetool.server.HTTPServer"
                          "--port" (format "%s" flycheck-languagetool-server-port))))
           (process
            (apply #'start-process
                   "languagetool-server"
                   " *LanguageTool server*"
                   (append cmd flycheck-languagetool-server-args))))
      (set-process-query-on-exit-flag process nil)
      (while
          (with-current-buffer (process-buffer process)
            (goto-char (point-min))
            (unless (re-search-forward " Server started$" nil t)
              (accept-process-output process 1)
              (process-live-p process)))))))

(defun flycheck-languagetool--json-org-line (line-str &optional has-nl nl-str)
  "Parse LINE-STR as Org text to JSON.
When the line has newline char (non-nil HAS-NL), add newline (NL-STR) as
markup."
  (let (annos)
    (if (and has-nl (string-match "^[ \t]*$" line-str))
        ;; Empty line breaks up sentences/paragraphs.
        (push `((markup . ,(concat (match-string 0 line-str) nl-str))
                (interpretAs . "\n\n"))
              annos)
      (let* ((_ (string-match "^\\(?1:[ \t]+\\)?\\(?2:.+\\)\\(?3:[ \t]+\\)?$" line-str))
             (pre (match-string 1 line-str))
             (text (match-string 2 line-str))
             (post (match-string 3 line-str)))
        (when pre
          (push `((markup . ,pre)) annos))
        (let ((beg 0)
              (org-link-re "\\[\\[\\([^][\n]+\\)\\]\\(?:\\[\\([^][\n]+\\)\\]\\)?\\]"))
          (while (string-match org-link-re text beg)
            (let ((m-beg (match-beginning 0))
                  (m-end (match-end 0))
                  (has-desc (match-beginning 2)))
              (when (> m-beg beg)
                (push `((text . ,(substring text beg m-beg))) annos))
              (if has-desc
                  (let ((desc-start (match-beginning 2))
                        (desc-end (match-end 2)))
                    (push `((markup . ,(substring text m-beg desc-start))) annos)
                    (push `((text . ,(substring text desc-start desc-end))) annos)
                    (push `((markup . ,(substring text desc-end m-end))) annos))
                (push `((markup . ,(substring text m-beg m-end))) annos))
              (setq beg m-end)))
          (when (< beg (length text))
            (push `((text . ,(substring text beg))) annos)))
        (when post
          (push `((markup . ,post)) annos)))
      (when has-nl       ; soft return / forced line break
        (push `((markup . ,nl-str)) annos)))
    annos))

(defun flycheck-languagetool--json-org (beg end)
  "Convert Org buffer region from BEG to END to annotated JSON."
  (save-excursion
    (goto-char beg)
    (let ((org-header-re
           (concat "^\\(?1:\\*+\\)"
                   "\\(?2: +\\(?3:"
                   (regexp-opt (if (bound-and-true-p org-todo-keywords-1)
                                   org-todo-keywords-1
                                 '("TODO" "DONE")))
                   "\\)\\)?"
                   "\\(?4: +\\(?5:\\[\\#\\(?6:[A-Z]\\|[0-9]\\|[1-5][0-9]\\|6[0-4]\\)\\]\\)\\)?"
                   "\\(?7:\\(?8: +\\)\\(?9:.*?\\)\\)??"
                   "\\(?10:[ \t]+\\(?11::\\([[:alnum:]_@#%:]+\\):\\)\\)?"
                   "\\(?12:[ \t]*\\)$"))
          (org-list-re
           (concat "^\\(?1:[ \t]*\\(?:[-+*]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ \t]+\\|$\\)\\)"
	           "\\(?5:\\[@\\(?:start:\\)?\\(?:[0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
	           "\\(?8:\\(?:\\[[ X-]\\]\\)\\(?:[ \t]+\\|$\\)\\)?"
	           "\\(?11:\\(?12:.+\\)\\(?13:[ \t]+::[ \t]+\\)\\)?"
                   "\\(?15:.+\\)$"))
          annos in-org-block)
      (while (< (point) end)
        (let* ((line-beg (point))
               (line-end (line-end-position))
               (line-str (buffer-substring-no-properties line-beg line-end))
               (has-nl (< line-end end))
               (nl-str (if has-nl "\n" "")))
          (cond
           ;; Block entry and exit (export, center, comment, example, quote, src, verse)
           ((string-match-p "^[ \t]*#\\+BEGIN_\\(COMMENT\\|EXAMPLE\\|EXPORT\\|SRC\\)" line-str)
            (setq in-org-block 'markup)
            (push `((markup . ,(concat line-str nl-str)) (interpretAs . "\n\n")) annos))
           ((string-match-p "^[ \t]*#\\+BEGIN_\\(CENTER\\|QUOTE\\|VERSE\\)" line-str)
            (setq in-org-block 'text)
            (push `((markup . ,(concat line-str nl-str)) (interpretAs . "\n\n")) annos))
           ((string-match-p "^[ \t]*#\\+END_[A-Z]+" line-str)
            (setq in-org-block nil)
            (push `((markup . ,(concat line-str nl-str)) (interpretAs . "\n\n")) annos))
           ((eq in-org-block 'markup)
            (push `((markup . ,line-str)) annos))
           ((eq in-org-block 'text)
            (setq annos (nconc (flycheck-languagetool--json-org-line line-str has-nl nl-str) annos)))

           ;; Structural elements (keywords, drawers, comments, property lines)
           ((string-match-p "^[ \t]*\\(#\\+\\|:\\|# \\|-----\\)" line-str)
            (push `((markup . ,(concat line-str nl-str)) (interpretAs . "\n\n")) annos))

           ;; Org headline
           ((string-match org-header-re line-str)
            (let ((stars (match-string 1 line-str))
                  (todo (match-string 2 line-str))
                  (priority (match-string 4 line-str))
                  (text (match-string 9 line-str))
                  (tags (match-string 10 line-str))
                  (tail (match-string 12 line-str)))
              (push `((markup . ,stars) (interpretAs . "\n\n")) annos)
              (when todo
                (push `((markup . ,todo)) annos))
              (when priority
                (push `((markup . ,priority)) annos))
              (when text
                (push `((markup . ,(match-string 8 line-str))) annos)
                (setq annos (nconc (flycheck-languagetool--json-org-line text) annos)))
              (when tags
                (push `((markup . ,tags)) annos))
              (unless (string-empty-p tail)
                (push `((markup . ,tail)) annos))
              (when has-nl
                (push `((markup . ,nl-str) (interpretAs . "\n\n")) annos))))

           ;; Org list
           ((string-match org-list-re line-str)
            (let ((bullet (match-string 1 line-str))
                  (counter (match-string 5 line-str))
                  (checkbox (match-string 8 line-str))
                  (term (match-string 12 line-str))
                  (colons (match-string 13 line-str))
                  (desc (match-string 15 line-str)))
              (push `((markup . ,bullet) (interpretAs . "\n\n")) annos)
              (when counter
                (push `((markup . ,counter)) annos))
              (when checkbox
                (push `((markup . ,checkbox)) annos))
              (when term
                (setq annos (nconc (flycheck-languagetool--json-org-line term) annos)))
              (when colons
                (push `((markup . ,colons) (interpretAs . "\n\n")) annos))
              (setq annos (nconc (flycheck-languagetool--json-org-line desc has-nl nl-str) annos))))
           (t
            (setq annos (nconc (flycheck-languagetool--json-org-line line-str has-nl nl-str) annos)))))
        (forward-line 1))
      (json-encode `((annotation . ,(vconcat (nreverse annos))))))))

(defun flycheck-languagetool--start (_checker callback)
  "Flycheck start function for _CHECKER `languagetool', invoking CALLBACK."
  (when (or flycheck-languagetool-server-command
            flycheck-languagetool-server-jar)
    (unless flycheck-languagetool--started-server
      (setq flycheck-languagetool--started-server t)
      (flycheck-languagetool--start-server)))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/x-www-form-urlencoded")))
         (disabled-rules
          (flatten-tree (list
                         (cdr (assoc "disabledRules"
                                     flycheck-languagetool-check-params))
                         (when (or (bound-and-true-p flyspell-mode)
                                   (bound-and-true-p jinx-mode))
                           flycheck-languagetool--spelling-rules))))
         (other-params (assoc-delete-all "disabledRules"
                                         (copy-alist flycheck-languagetool-check-params)))
         (url-request-data
          (mapconcat
           (lambda (param)
             (concat (url-hexify-string (car param)) "="
                     (url-hexify-string (cdr param))))
           (append other-params
                   `(("language" . ,flycheck-languagetool-language))
                   (cond
                    ((derived-mode-p 'org-mode)
                     `(("data" . ,(flycheck-languagetool--json-org
                                   (point-min) (point-max)))))
                    (t
                     `(("text" . ,(buffer-substring-no-properties
                                   (point-min) (point-max))))))
                   (when disabled-rules
                     (list (cons "disabledRules"
                                 (string-join disabled-rules ",")))))
           "&")))
    (url-retrieve
     (concat (or flycheck-languagetool-url
                 (format "http://localhost:%s"
                         flycheck-languagetool-server-port))
             "/v2/check")
     #'flycheck-languagetool--read-results
     (list (current-buffer) (buffer-chars-modified-tick) callback)
     t)))

(defun flycheck-languagetool--error-explainer (err)
  "Link to a detailed explanation of ERR on the LanguageTool website."
  (let* ((error-id (flycheck-error-id err))
         (id (car error-id))
         (subid (cdr error-id))
         (url (apply #'format
                     "https://community.languagetool.org/rule/show/%s?lang=%s"
                     (mapcar #'url-hexify-string
                             (list id flycheck-languagetool-language)))))
    (when subid
      (setq url (concat url
                        (format "&subId=%s" (url-hexify-string subid)))))
    `(url . ,url)))

(defun flycheck-languagetool--enabled ()
  "Can the Flycheck LanguageTool checker be enabled?"
  (cond (flycheck-languagetool-url
         (not (string= "" flycheck-languagetool-url)))
        (flycheck-languagetool-server-command
         (and (listp flycheck-languagetool-server-command)
              (executable-find (car flycheck-languagetool-server-command))))
        (flycheck-languagetool-server-jar
         (and (not (string= "" flycheck-languagetool-server-jar))
              (file-exists-p flycheck-languagetool-server-jar)
              (executable-find "java")))))

(defun flycheck-languagetool--verify (_checker)
  "Verify proper configuration of Flycheck _CHECKER `languagetool'."
  (list
   (flycheck-verification-result-new
    ;; We could improve this test by also checking that we can
    ;; successfully make requests to the URL.
    :label "LanguageTool API URL"
    :message (if flycheck-languagetool-url
                 (if (not (string= "" flycheck-languagetool-url))
                     flycheck-languagetool-url "Blank")
               "Not configured")
    :face (if flycheck-languagetool-url
              (if (not (string= "" flycheck-languagetool-url))
                  'success '(bold error))
            '(bold warning)))
   (flycheck-verification-result-new
    :label "LanguageTool server command"
    :message
    (if flycheck-languagetool-server-command
        (format (if (and (executable-find
                          (car flycheck-languagetool-server-command)))
                    "Found at %s" "Configured as %s but missing")
                (car flycheck-languagetool-server-command))
      "Not configured")
    :face (if flycheck-languagetool-server-command
              (if (and (listp flycheck-languagetool-server-command)
                       (executable-find
                        (car flycheck-languagetool-server-command)))
                  'success '(bold error))
            '(bold warning)))
   (flycheck-verification-result-new
    :label "LanguageTool server JAR"
    :message
    (if flycheck-languagetool-server-jar
        (format (if (and (not (string= "" flycheck-languagetool-server-jar))
                         (file-exists-p flycheck-languagetool-server-jar))
                    "Found at %s" "Missing from %s")
                flycheck-languagetool-server-jar)
      "Not configured")
    :face (if flycheck-languagetool-server-jar
              (if (and (not (string= "" flycheck-languagetool-server-jar))
                       (file-exists-p flycheck-languagetool-server-jar))
                  'success '(bold error))
            '(bold warning)))
   (flycheck-verification-result-new
    :label "Java executable"
    :message (or (executable-find "java") "Not found")
    :face (if (executable-find "java") 'success '(bold warning)))))

(flycheck-define-generic-checker 'languagetool
  "LanguageTool flycheck definition."
  :start #'flycheck-languagetool--start
  :enabled #'flycheck-languagetool--enabled
  :verify #'flycheck-languagetool--verify
  :error-explainer #'flycheck-languagetool--error-explainer
  :modes flycheck-languagetool-active-modes
  :next-checkers '(proselint))

;;;###autoload
(defun flycheck-languagetool-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers 'languagetool))

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
