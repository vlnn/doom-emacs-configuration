;;; test/ai-flymake-test.el -*- lexical-binding: t; -*-

(require 'flymake)

(defmacro +ai-flymake-test--in-buffer (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     ,@body))

(defun +ai-flymake-test--reports ()
  "A report function that records its calls, and a way to read them back."
  (let (calls)
    (list (lambda (&rest args) (push args calls))
          (lambda () (reverse calls)))))

;;; parsing

(ert-deftest +ai--flymake-parse/reads-line-severity-and-message ()
  (ert-info ("parse should split `N: SEVERITY: message' into line, type and text")
    (should (equal '((3 :warning "unused binding") (7 :error "nil deref"))
                   (+ai--flymake-parse "3: warning: unused binding\n7: error: nil deref")))))

(ert-deftest +ai--flymake-parse/defaults-missing-severity-to-note ()
  (ert-info ("parse should treat a finding without a severity word as a note")
    (should (equal '((4 :note "Unused variable: x"))
                   (+ai--flymake-parse "4: Unused variable: x")))))

(ert-deftest +ai--flymake-parse/accepts-case-and-separators ()
  (ert-info ("parse should accept any case and N. or N) like the annotate parser")
    (should (equal '((1 :note "a") (2 :error "b") (3 :warning "c"))
                   (+ai--flymake-parse "1. NOTE: a\n 2) Error: b\n3: WARNING: c")))))

(ert-deftest +ai--flymake-parse/drops-prose-and-fences ()
  (ert-info ("parse should ignore lines that are not findings")
    (should (equal '((2 :note "x"))
                   (+ai--flymake-parse "```\nHere are my findings:\n2: note: x\n```")))))

(ert-deftest +ai--flymake-parse/empty-response-means-no-findings ()
  (ert-info ("parse should return nil for an empty response")
    (should (null (+ai--flymake-parse "")))))

;;; diagnostics

(ert-deftest +ai--flymake-diagnostics/covers-the-named-line ()
  (+ai-flymake-test--in-buffer "a = 1\n  b = 2\nc = 3\n"
    (let ((diag (car (+ai--flymake-diagnostics (current-buffer) 'llama "2: warning: shadows"))))
      (ert-info ("diagnostics should start at the first non-blank column of the line")
        (should (= 9 (flymake-diagnostic-beg diag))))
      (ert-info ("diagnostics should end at the end of the line")
        (should (= 14 (flymake-diagnostic-end diag))))
      (ert-info ("diagnostics should carry the parsed severity")
        (should (eq :warning (flymake-diagnostic-type diag))))
      (ert-info ("diagnostics should name the provider before the message")
        (should (equal "llama: shadows" (flymake-diagnostic-text diag)))))))

(ert-deftest +ai--flymake-diagnostics/drops-lines-outside-the-buffer ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (ert-info ("diagnostics should drop findings past the last line or at line 0")
      (should (= 1 (length (+ai--flymake-diagnostics
                            (current-buffer) 'claude "0: error: x\n2: note: y\n9: error: z")))))))

;;; providers

(ert-deftest +ai--flymake-next-provider/cycles-llama-and-claude ()
  (ert-info ("llama should be followed by claude")
    (should (eq 'claude (+ai--flymake-next-provider 'llama))))
  (ert-info ("claude should wrap around to llama")
    (should (eq 'llama (+ai--flymake-next-provider 'claude)))))

(ert-deftest +ai-flymake-provider/defaults-to-llama ()
  (ert-info ("the default provider should be the local llama-server")
    (should (eq 'llama (default-value '+ai-flymake-provider)))))

(ert-deftest +ai--flymake-provider/rejects-unknown-names ()
  (ert-info ("an unknown provider should be an error, not a silent fallback")
    (should-error (+ai--flymake-provider 'bard))))

;;; replies

(ert-deftest +ai--flymake-callback/reports-diagnostics-for-the-current-run ()
  (+ai-flymake-test--in-buffer "x\ny\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 3)
      (funcall (+ai--flymake-callback (current-buffer) 3 report 'llama) "2: error: boom" nil)
      (ert-info ("the callback should hand flymake one diagnostic per finding")
        (should (= 1 (length (car (car (funcall calls)))))))
      (ert-info ("the diagnostic should carry the finding, signed by the provider")
        (should (equal "llama: boom" (flymake-diagnostic-text (car (car (car (funcall calls)))))))))))

(ert-deftest +ai--flymake-callback/drops-replies-to-superseded-runs ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 4)
      (funcall (+ai--flymake-callback (current-buffer) 3 report 'llama) "1: error: old" nil)
      (ert-info ("a reply for an earlier run should not reach flymake")
        (should (null (funcall calls)))))))

(ert-deftest +ai--flymake-callback/panics-when-the-request-fails ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 1)
      (funcall (+ai--flymake-callback (current-buffer) 1 report 'claude) nil '(:status "401"))
      (ert-info ("a failed request should panic the backend so flymake shows it")
        (should (eq :panic (car (car (funcall calls))))))
      (ert-info ("the panic should name the provider and the status")
        (should (string-match-p "claude.*401" (plist-get (cdr (car (funcall calls))) :explanation)))))))

(ert-deftest +ai--flymake-callback/ignores-a-killed-buffer ()
  (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports))
              (buffer (generate-new-buffer "dead")))
    (kill-buffer buffer)
    (funcall (+ai--flymake-callback buffer 1 report 'llama) "1: note: x" nil)
    (ert-info ("a reply for a buffer that is gone should be dropped")
      (should (null (funcall calls))))))

;;; size guard

(ert-deftest +ai--flymake-too-long-p/compares-line-count-with-the-limit ()
  (+ai-flymake-test--in-buffer "1\n2\n3\n"
    (let ((+ai-flymake-max-lines 2))
      (ert-info ("three lines should be too long for a limit of two")
        (should (+ai--flymake-too-long-p))))
    (let ((+ai-flymake-max-lines 3))
      (ert-info ("three lines should fit a limit of three")
        (should-not (+ai--flymake-too-long-p))))))

;;; progress

(ert-deftest +ai--flymake-lighter/marks-a-review-in-flight ()
  (+ai-flymake-test--in-buffer ""
    (setq +ai--flymake-pending nil)
    (ert-info ("the lighter should be plain while nothing is pending")
      (should (equal " AIrev" (+ai--flymake-lighter))))
    (setq +ai--flymake-pending 'llama)
    (ert-info ("the lighter should name the reviewer being waited on")
      (should (equal " AIrev[llama…]" (+ai--flymake-lighter))))))

(ert-deftest +ai--flymake-callback/clears-pending-and-counts-findings ()
  (+ai-flymake-test--in-buffer "x\ny\n"
    (pcase-let ((`(,report ,_) (+ai-flymake-test--reports))
                (said nil))
      (setq +ai--flymake-run 1
            +ai--flymake-pending 'claude)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (funcall (+ai--flymake-callback (current-buffer) 1 report 'claude) "1: note: a\n2: error: b" nil))
      (ert-info ("a reply should clear the pending marker")
        (should (null +ai--flymake-pending)))
      (ert-info ("a reply should say who reviewed and how many findings landed")
        (should (equal "AI review by claude: 2 findings" said))))))

(ert-deftest +ai--flymake-callback/says-when-nothing-was-found ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,_) (+ai-flymake-test--reports))
                (said nil))
      (setq +ai--flymake-run 1 +ai--flymake-pending 'llama)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (funcall (+ai--flymake-callback (current-buffer) 1 report 'llama) "" nil))
      (ert-info ("an empty reply should be reported as a clean review, not silence")
        (should (equal "AI review by llama: nothing to flag" said))))))

(ert-deftest +ai--flymake-callback/clears-pending-on-failure ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,_) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 1 +ai--flymake-pending 'llama)
      (funcall (+ai--flymake-callback (current-buffer) 1 report 'llama) nil '(:status "500"))
      (ert-info ("a failed reply should also clear the pending marker")
        (should (null +ai--flymake-pending))))))

(ert-deftest +ai--flymake-findings-text/pluralizes ()
  (ert-info ("one finding should be singular")
    (should (equal "1 finding" (+ai--flymake-findings-text 1))))
  (ert-info ("several findings should be plural")
    (should (equal "3 findings" (+ai--flymake-findings-text 3)))))
