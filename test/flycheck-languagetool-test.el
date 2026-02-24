;;; flycheck-languagetool-test.el --- Tests  -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for flycheck-languagetool.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'flycheck-languagetool)

;; ---------------------------------------------------------------------------
;; flycheck-languagetool--read-results — callback semantics
;; ---------------------------------------------------------------------------

(ert-deftest flt-test-read-results/dead-buffer ()
  "When the source buffer is dead, callback receives `interrupted'."
  (let (callback-args)
    (let ((dead-buf (generate-new-buffer " *flt-dead*")))
      (kill-buffer dead-buf)
      ;; Simulate url-retrieve response buffer
      (with-temp-buffer
        (let ((url-http-end-of-headers (point-min)))
          (insert "{\"matches\":[]}")
          (flycheck-languagetool--read-results
           nil dead-buf
           (lambda (status &optional errors)
             (setq callback-args (list status errors)))))))
    (should (eq 'interrupted (nth 0 callback-args)))))

(ert-deftest flt-test-read-results/callback-called-once-on-error ()
  "When parsing fails, callback is called exactly once with `errored'."
  (let ((call-count 0)
        callback-statuses)
    (let ((source-buf (generate-new-buffer " *flt-source*")))
      (unwind-protect
          (with-current-buffer source-buf
            (insert "short")
            ;; Create a separate buffer to simulate the url-retrieve
            ;; response buffer with malformed data.
            (let ((response-buf (generate-new-buffer " *flt-response*")))
              (unwind-protect
                  (progn
                    (with-current-buffer response-buf
                      (insert "{\"matches\": \"not-an-array\"}")
                      (setq-local url-http-end-of-headers (point-min))
                      (flycheck-languagetool--read-results
                       nil source-buf
                       (lambda (status &optional errors)
                         (cl-incf call-count)
                         (push status callback-statuses)))))
                ;; response-buf is killed by --read-results, but be safe
                (when (buffer-live-p response-buf)
                  (kill-buffer response-buf)))))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))))
    (should (= 1 call-count))
    (should (eq 'errored (car callback-statuses)))))

(ert-deftest flt-test-read-results/successful-parse ()
  "A well-formed response calls callback once with `finished'."
  (let ((call-count 0)
        callback-status
        callback-errors)
    (let ((source-buf (generate-new-buffer " *flt-source*")))
      (unwind-protect
          (with-current-buffer source-buf
            (insert "Hello, world!")
            (let ((response-buf (generate-new-buffer " *flt-response*")))
              (unwind-protect
                  (progn
                    (with-current-buffer response-buf
                      ;; json-encode with json-array-type doesn't
                      ;; matter here; flycheck-parse-json handles it.
                      (let ((json-str (json-encode
                                       '((matches . [((offset . 0)
                                                      (length . 5)
                                                      (message . "test msg")
                                                      (rule . ((id . "R1"))))])))))
                        (insert json-str))
                      (setq-local url-http-end-of-headers (point-min))
                      (flycheck-languagetool--read-results
                       nil source-buf
                       (lambda (status &optional errors)
                         (cl-incf call-count)
                         (setq callback-status status
                               callback-errors errors)))))
                (when (buffer-live-p response-buf)
                  (kill-buffer response-buf)))))
        (when (buffer-live-p source-buf)
          (kill-buffer source-buf))))
    (should (= 1 call-count))
    (should (eq 'finished callback-status))
    (should (= 1 (length callback-errors)))))

(provide 'flycheck-languagetool-test)
;;; flycheck-languagetool-test.el ends here
