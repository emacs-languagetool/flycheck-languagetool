;;; flycheck-languagetool.el --- Flycheck support for LanguageTool  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shen, Jen-Chieh
;; Created date 2021-04-02 23:22:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Flycheck support for LanguageTool.
;; Keyword: grammar check
;; Version: 0.2.0
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

(defcustom flycheck-languagetool-url "http://localhost:8081"
  "The URL for the LanguageTool API we should connect to."
  :type 'string
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-jar ""
  "The path of languagetool-server.jar."
  :type '(file :must-match t)
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-server-port 8081
  "The port on which a server should listen."
  :type 'integer
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-language "en-US"
  "The language code of the text to check."
  :type '(string :tag "Language")
  :safe #'stringp
  :group 'flycheck-languagetool)
(make-variable-buffer-local 'flycheck-languagetool-language)

(defcustom flycheck-languagetool-args ""
  "Extra argument pass in to command line tool."
  :type 'string
  :group 'flycheck-languagetool)

(defcustom flycheck-languagetool-check-time 0.8
  "How long do we call process after we done typing."
  :type 'float
  :group 'flycheck-languagetool)

(defvar-local flycheck-languagetool--done-checking t
  "If non-nil then we are currnetly in the checking process.")

(defvar-local flycheck-languagetool--timer nil
  "Timer that will tell to do the request.")

(defvar-local flycheck-languagetool--output nil
  "Copy of the JSON output.")

(defvar-local flycheck-languagetool--source-buffer nil
  "Current buffer we are currently using for grammar check.")

;;
;; (@* "Util" )
;;

(defun flycheck-languagetool--column-at-pos (&optional pt)
  "Return column at PT."
  (unless pt (setq pt (point)))
  (save-excursion (goto-char pt) (current-column)))

(defmacro flycheck-languagetool--with-source-buffer (&rest body)
  "Execute BODY inside currnet source buffer."
  (declare (indent 0) (debug t))
  `(if flycheck-languagetool--source-buffer
       (with-current-buffer flycheck-languagetool--source-buffer (progn ,@body))
     (user-error "Invalid source buffer: %s" flycheck-languagetool--source-buffer)))

;;
;; (@* "Core" )
;;

(defun flycheck-languagetool--check-all ()
  "Check grammar for buffer document."
  (let ((matches (cdr (assoc 'matches flycheck-languagetool--output)))
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

(defun flycheck-languagetool--cache-parse-result (output)
  "Refressh cache buffer from OUTPUT."
  (setq flycheck-languagetool--output (car (flycheck-parse-json output))
        flycheck-languagetool--done-checking t)
  (flycheck-buffer-automatically))

(defun flycheck-languagetool--read-result (status source-buffer)
  "Callback for results from LanguageTool API.

STATUS is passed from `url-retrieve'.
SOURCE-BUFFER is the buffer currently being checked."
  (set-buffer-multibyte t)
  (search-forward "\n\n")
  (let ((output (buffer-substring (point) (point-max))))
    (with-current-buffer source-buffer
      (flycheck-languagetool--cache-parse-result output)))
  (kill-buffer))

(defun flycheck-languagetool--send-process ()
  "Send process to LanguageTool commandline-jar."
  (when flycheck-languagetool--done-checking
    (setq flycheck-languagetool--done-checking nil)  ; start flag
    (flycheck-languagetool--with-source-buffer
      (let ((url-request-method "POST")
            (url-request-extra-headers
             '(("Content-Type" . "application/x-www-form-urlencoded")))
            (url-request-data
             (concat
              "language=" (url-hexify-string flycheck-languagetool-language)
              "&text=" (url-hexify-string (buffer-string)))))
        (url-retrieve (concat flycheck-languagetool-url "/v2/check")
                      #'flycheck-languagetool--read-result
                      (list (current-buffer))
                      t)))))

(defun flycheck-languagetool--start-timer ()
  "Start the timer for grammar check."
  (setq flycheck-languagetool--source-buffer (current-buffer))
  (when (timerp flycheck-languagetool--timer) (cancel-timer flycheck-languagetool--timer))
  (setq flycheck-languagetool--timer
        (run-with-idle-timer flycheck-languagetool-check-time nil
                             #'flycheck-languagetool--send-process)))

(defun flycheck-languagetool--start-server ()
  "Start the LanguageTool server if we didn’t already."
  (unless (process-live-p (get-process "languagetool-server"))
    (set-process-query-on-exit-flag
     (start-process
      "languagetool-server"
      " *LanguageTool server*"
      "java" "-cp" (expand-file-name flycheck-languagetool-server-jar)
      "org.languagetool.server.HTTPServer"
      "--port" (format "%s" flycheck-languagetool-server-port))
     nil)))

(defun flycheck-languagetool--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (when flycheck-languagetool-server-jar
    (flycheck-languagetool--start-server))
  (flycheck-languagetool--start-timer)
  (funcall
   callback 'finished
   (flycheck-increment-error-columns
    (mapcar
     (lambda (x)
       (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
     (condition-case err
         (if flycheck-languagetool--done-checking
             (flycheck-languagetool--check-all)
           (flycheck-stop))
       (error (funcall callback 'errored (error-message-string err))
              (signal (car err) (cdr err))))))))

(flycheck-define-generic-checker 'languagetool
  "LanguageTool flycheck definition."
  :start #'flycheck-languagetool--start
  :modes flycheck-languagetool-active-modes)

(add-to-list 'flycheck-checkers 'languagetool)

(provide 'flycheck-languagetool)
;;; flycheck-languagetool.el ends here
