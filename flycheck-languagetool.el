;;; flycheck-languagetool.el --- Flycheck support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Flycheck support for LanguageTool.
;; Keyword: grammar check
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1") (flycheck "0.14") (s "1.9.0"))
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

(defcustom flycheck-languagetool-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with LanguageTool."
  :type 'list
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-url nil
  "The URL for the LanguageTool API we should connect to."
  :type '(choice (const :tag "Auto" nil)
                 (string :tag "URL"))
  :package-version '(flycheck-languagetool . "0.3.0")
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
  :link '(url-link
          :tag "LanguageTool API"
          "https://languagetool.org/http-api/swagger-ui/#!/default/post_check")
  :group 'flycheck-languagetool)

;;
;; (@* "Util" )
;;

(defun flycheck-languagetool--column-at-pos (&optional pt)
  "Return column at PT."
  (unless pt (setq pt (point)))
  (save-excursion (goto-char pt) (current-column)))

;;
;; (@* "Core" )
;;

(defun flycheck-languagetool--check-all (output)
  "Check grammar for buffer document."
  (let ((matches (cdr (assoc 'matches output)))
        check-list)
    (dolist (match matches)
      (let* ((pt-beg (+ 1 (cdr (assoc 'offset match))))
             (len (cdr (assoc 'length match)))
             (pt-end (+ pt-beg len))
             (ln (line-number-at-pos pt-beg))
             (type 'warning)
             (desc (cdr (assoc 'message match)))
             (col-start (flycheck-languagetool--column-at-pos pt-beg))
             (col-end (flycheck-languagetool--column-at-pos pt-end)))
        (push (list ln col-start type desc :end-column col-end)
              check-list)))
    (progn  ; Remove fitst and last element to avoid quote warnings
      (pop check-list)
      (setq check-list (butlast check-list)))
    check-list))

(defun flycheck-languagetool--read-result (status source-buffer callback)
  "Callback for results from LanguageTool API.

STATUS is passed from `url-retrieve'.
SOURCE-BUFFER is the buffer currently being checked.
CALLBACK is passed from Flycheck."
  (let ((err (plist-get status :error)))
    (when err
      (kill-buffer)
      (error (funcall callback 'errored (error-message-string err))
             (signal (car err) (cdr err)))))

  (set-buffer-multibyte t)
  (search-forward "\n\n")
  (let ((output (car (flycheck-parse-json
                      (buffer-substring (point) (point-max))))))
    (kill-buffer)
    (with-current-buffer source-buffer
      (funcall
       callback 'finished
       (flycheck-increment-error-columns
        (mapcar
         (lambda (x)
           (apply #'flycheck-error-new-at `(,@x :checker languagetool)))
         (condition-case err
             (flycheck-languagetool--check-all output)
           (error (funcall callback 'errored (error-message-string err))
                  (signal (car err) (cdr err))))))))))

(defun flycheck-languagetool--start-server ()
  "Start the LanguageTool server if we didn’t already."
  (unless (process-live-p (get-process "languagetool-server"))
    (set-process-query-on-exit-flag
     (apply
      #'start-process
      "languagetool-server"
      " *LanguageTool server*"
      "java" "-cp" (expand-file-name flycheck-languagetool-server-jar)
      "org.languagetool.server.HTTPServer"
      "--port" (format "%s" flycheck-languagetool-server-port)
      flycheck-languagetool-server-args)
     nil)))

(defun flycheck-languagetool--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (when flycheck-languagetool-server-jar
    (flycheck-languagetool--start-server))

  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat
          (lambda (param)
            (concat (url-hexify-string (car param)) "="
                    (url-hexify-string (cdr param))))
          (append flycheck-languagetool-check-params
                  `(("language" . ,flycheck-languagetool-language)
                    ("text" . ,(buffer-string))))
          "&")))
    (url-retrieve
     (concat (or flycheck-languagetool-url
                 (format "http://localhost:%s"
                         flycheck-languagetool-server-port))
             "/v2/check")
     #'flycheck-languagetool--read-result
     (list (current-buffer) callback)
     t)))

(defun flycheck-languagetool--enabled ()
  "Can the Flycheck LanguageTool checker be enabled?"
  (or (and flycheck-languagetool-server-jar
           (not (string= "" flycheck-languagetool-server-jar))
           (file-exists-p flycheck-languagetool-server-jar))
      (and flycheck-languagetool-url
           (not (string= "" flycheck-languagetool-url)))))

(defun flycheck-languagetool--verify (checker)
  "Verify proper configuration of Flycheck CHECKER `languagetool'."
  (list
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
            '(bold warning)))))

(flycheck-define-generic-checker 'languagetool
  "LanguageTool flycheck definition."
  :start #'flycheck-languagetool--start
  :enabled #'flycheck-languagetool--enabled
  :verify #'flycheck-languagetool--verify
  :modes flycheck-languagetool-active-modes)

(add-to-list 'flycheck-checkers 'languagetool)

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
