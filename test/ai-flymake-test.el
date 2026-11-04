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

(ert-deftest +ai--flymake-callback/a-failed-request-ends-the-run-without-disabling-the-backend ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports))
                (said nil))
      (setq +ai--flymake-run 1)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args))))
                ((symbol-function 'flymake-log) #'ignore))
        (funcall (+ai--flymake-callback (current-buffer) 1 report 'claude)
                 nil '(:status "401" :error "invalid x-api-key")))
      (ert-info ("a failed request should report no findings, never :panic, so the next save tries again")
        (should (equal '((nil)) (funcall calls))))
      (ert-info ("the failure should be said aloud with provider, status and the API's reason")
        (should (equal "AI review by claude failed: 401 invalid x-api-key" said))))))

(ert-deftest +ai--flymake-failure-text/copes-with-missing-details ()
  (ert-info ("a failure with no error text should still read")
    (should (equal "AI review by llama failed: 500" (+ai--flymake-failure-text 'llama '(:status "500")))))
  (ert-info ("a failure with nothing known should say so")
    (should (equal "AI review by llama failed: no response" (+ai--flymake-failure-text 'llama nil)))))

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
      (cl-letf (((symbol-function 'message) #'ignore)
                ((symbol-function 'flymake-log) #'ignore))
        (funcall (+ai--flymake-callback (current-buffer) 1 report 'llama) nil '(:status "500")))
      (ert-info ("a failed reply should also clear the pending marker")
        (should (null +ai--flymake-pending))))))

(ert-deftest +ai--flymake-findings-text/pluralizes ()
  (ert-info ("one finding should be singular")
    (should (equal "1 finding" (+ai--flymake-findings-text 1))))
  (ert-info ("several findings should be plural")
    (should (equal "3 findings" (+ai--flymake-findings-text 3)))))

;;; rendering

(defun +ai-flymake-test--overlays ()
  (seq-filter (lambda (o) (overlay-get o '+ai-flymake)) (overlays-in (point-min) (point-max))))

(ert-deftest +ai--flymake-parse/knows-ideas ()
  (ert-info ("parse should map `idea' to the idea type")
    (should (equal '((2 +ai-flymake-idea "inline this")) (+ai--flymake-parse "2: idea: inline this")))))

(ert-deftest +ai-flymake-idea/counts-as-a-note-for-flymake ()
  (ert-info ("ideas should sit in flymake's note category so they never inflate the error count")
    (should (eq 'flymake-note (get '+ai-flymake-idea 'flymake-category)))))

(ert-deftest +ai--flymake-summary/joins-one-line-s-findings-without-the-provider ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (let ((text (+ai--flymake-summary
                 (+ai--flymake-diagnostics (current-buffer) 'llama "1: error: x\n1: note: y"))))
      (ert-info ("the summary should carry every finding of the line, separated, provider dropped")
        (should (equal "x │ y" (substring-no-properties text))))
      (ert-info ("the summary should wear the face of the worst finding")
        (should (eq 'flymake-error-echo (get-text-property 0 'face text)))))))

(ert-deftest +ai--flymake-summary/ideas-rank-below-notes ()
  (+ai-flymake-test--in-buffer "a\n"
    (let ((text (+ai--flymake-summary
                 (+ai--flymake-diagnostics (current-buffer) 'llama "1: idea: x\n1: note: y"))))
      (ert-info ("a note should outrank an idea for the line's face")
        (should (eq 'flymake-note-echo (get-text-property 0 'face text)))))))

(defun +ai-flymake-test--render-eol (native-eol)
  (cl-letf (((symbol-function '+ai--flymake-native-eol-p) (lambda () native-eol)))
    (let ((+ai-flymake-display 'eol))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x\n3: error: y\n3: note: z")))))

(ert-deftest +ai--flymake-render/eol-draws-its-own-overlays-when-flymake-cannot ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (+ai-flymake-test--render-eol nil)
    (let ((overlays (+ai-flymake-test--overlays)))
      (ert-info ("two annotated lines should give two overlays")
        (should (= 2 (length overlays))))
      (ert-info ("each overlay should sit at the end of its line")
        (should (equal '(2 6) (sort (mapcar #'overlay-start overlays) #'<))))
      (ert-info ("the eol text should point back at the line")
        (should (string-prefix-p "  ⟵ " (overlay-get (car overlays) 'after-string)))))))

(ert-deftest +ai--flymake-render/eol-leaves-the-lines-to-flymake-when-it-can ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (+ai-flymake-test--render-eol t)
    (ert-info ("with flymake showing diagnostics at end of line itself, render should add no overlay")
      (should (null (+ai-flymake-test--overlays))))))

(ert-deftest +ai--flymake-diagnostic/keeps-line-and-message-as-data ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (let ((diag (car (+ai--flymake-diagnostics (current-buffer) 'llama "2: note: hi"))))
      (ert-info ("the data slot should hold the line and the bare message, for rendering without flymake")
        (should (equal '(2 . "hi") (flymake-diagnostic-data diag)))))))

(ert-deftest +ai--flymake-by-line/groups-by-the-finding-s-own-line ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (let ((groups (+ai--flymake-by-line
                   (+ai--flymake-diagnostics (current-buffer) 'llama "3: note: x\n1: note: y\n3: idea: z"))))
      (ert-info ("findings should be grouped by line number, in order")
        (should (equal '(1 3) (sort (mapcar #'car groups) #'<))))
      (ert-info ("a line should collect every finding about it")
        (should (= 2 (length (alist-get 3 groups))))))))

(ert-deftest +ai--flymake-render/column-aligns-each-finding-past-the-code ()
  (+ai-flymake-test--in-buffer "a\nbbbb\nc\n"
    (let ((+ai-flymake-display 'column)
          (+ai-flymake-column nil)
          (fill-column 10))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x\n3: idea: y")))
    (let* ((overlays (sort (+ai-flymake-test--overlays) (lambda (a b) (< (overlay-start a) (overlay-start b)))))
           (text (overlay-get (car overlays) 'after-string)))
      (ert-info ("each annotated line should get its own overlay at its end")
        (should (equal '(2 9) (mapcar #'overlay-start overlays))))
      (ert-info ("the overlay should start with a stretch aligned to the annotation column")
        (should (equal '(space :align-to 12) (get-text-property 0 'display text))))
      (ert-info ("the summary should follow the stretch")
        (should (equal " x" (substring-no-properties text 1)))))))

(ert-deftest +ai--flymake-column/sits-two-past-the-longest-line-or-fill-column ()
  (+ai-flymake-test--in-buffer "a\nbbbbbbbbbbbbbbb\n"
    (let ((+ai-flymake-column nil) (fill-column 5))
      (ert-info ("with short fill-column the longest line should decide")
        (should (= 17 (+ai--flymake-column)))))
    (let ((+ai-flymake-column nil) (fill-column 40))
      (ert-info ("a wider fill-column should win over the longest line")
        (should (= 42 (+ai--flymake-column)))))
    (let ((+ai-flymake-column 70))
      (ert-info ("a set column should be used as is")
        (should (= 70 (+ai--flymake-column)))))))

(ert-deftest +ai--flymake-render/margin-shows-the-summary-in-the-right-margin ()
  (+ai-flymake-test--in-buffer "a\nbb\n"
    (let ((+ai-flymake-display 'margin))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "2: warning: w")))
    (let* ((overlay (car (+ai-flymake-test--overlays)))
           (spec (get-text-property 0 'display (overlay-get overlay 'before-string))))
      (ert-info ("the margin overlay should start at the beginning of its line")
        (should (= 3 (overlay-start overlay))))
      (ert-info ("the string should be displayed in the right margin")
        (should (equal '(margin right-margin) (car spec))))
      (ert-info ("the margin should hold the summary")
        (should (equal "w" (substring-no-properties (cadr spec))))))))

(ert-deftest +ai--flymake-fit/truncates-to-the-margin ()
  (let ((+ai-flymake-margin-width 6))
    (ert-info ("a long summary should be cut with an ellipsis to the margin width")
      (should (equal "abcde…" (+ai--flymake-fit "abcdefghij"))))
    (ert-info ("a short summary should pass through")
      (should (equal "abc" (+ai--flymake-fit "abc"))))))

(ert-deftest +ai--flymake-clear/removes-only-ours ()
  (+ai-flymake-test--in-buffer "a\n"
    (let ((other (make-overlay 1 2))
          (+ai-flymake-display 'margin))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x"))
      (+ai--flymake-clear)
      (ert-info ("clearing should drop our overlays")
        (should (null (+ai-flymake-test--overlays))))
      (ert-info ("clearing should leave other overlays alone")
        (should (overlay-buffer other))))))

(ert-deftest +ai--flymake-report/replaces-earlier-annotations ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (pcase-let ((`(,report ,_) (+ai-flymake-test--reports))
                (+ai-flymake-display 'margin))
      (cl-letf (((symbol-function 'message) #'ignore))
        (+ai--flymake-report (current-buffer) 'llama report "1: note: old\n2: note: old")
        (+ai--flymake-report (current-buffer) 'llama report "2: note: new"))
      (ert-info ("a new report should leave exactly its own annotations")
        (should (= 1 (length (+ai-flymake-test--overlays))))))))

(ert-deftest +ai--flymake-report/leaves-eol-to-flymake-when-it-can ()
  (+ai-flymake-test--in-buffer "a\n"
    (pcase-let ((`(,report ,_) (+ai-flymake-test--reports))
                (+ai-flymake-display 'eol))
      (cl-letf (((symbol-function 'message) #'ignore)
                ((symbol-function '+ai--flymake-native-eol-p) (lambda () t)))
        (+ai--flymake-report (current-buffer) 'llama report "1: note: x"))
      (ert-info ("on an Emacs that shows diagnostics at end of line itself, we add no overlay")
        (should (null (+ai-flymake-test--overlays)))))))

(ert-deftest +ai--flymake-next-display/cycles-column-margin-eol ()
  (ert-info ("column should be followed by margin")
    (should (eq 'margin (+ai--flymake-next-display 'column))))
  (ert-info ("margin should be followed by eol")
    (should (eq 'eol (+ai--flymake-next-display 'margin))))
  (ert-info ("eol should wrap to column")
    (should (eq 'column (+ai--flymake-next-display 'eol)))))

(ert-deftest +ai-flymake-display/defaults-to-column ()
  (ert-info ("findings should default to a column just past the code")
    (should (eq 'column (default-value '+ai-flymake-display)))))

;;; every code buffer, and re-review on provider change

(defmacro +ai-flymake-test--with-mode-buffers (count &rest body)
  "Run BODY with COUNT live buffers that have the mode on, bound as `buffers'."
  (declare (indent 1))
  `(let ((buffers (cl-loop repeat ,count collect (generate-new-buffer "aireview"))))
     (unwind-protect
         (progn (dolist (b buffers) (with-current-buffer b (setq-local +ai-flymake-mode t)))
                ,@body)
       (mapc #'kill-buffer buffers))))

(ert-deftest +ai--flymake-reviewable-p/file-backed-code-buffers-only ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/x.el")
    (ert-info ("a file-visiting prog-mode buffer should be reviewed")
      (should (+ai--flymake-reviewable-p))))
  (with-temp-buffer
    (emacs-lisp-mode)
    (ert-info ("a scratch buffer with no file should not be reviewed")
      (should-not (+ai--flymake-reviewable-p))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/notes.txt")
    (ert-info ("a file that is not code should not be reviewed")
      (should-not (+ai--flymake-reviewable-p)))))

(ert-deftest +ai--flymake-buffers/finds-every-buffer-with-the-mode-on ()
  (+ai-flymake-test--with-mode-buffers 2
    (ert-info ("buffers with the mode on should all be found")
      (should (= 2 (length (seq-intersection buffers (+ai--flymake-buffers))))))))

(ert-deftest +ai/flymake-toggle-provider/re-reviews-every-buffer-with-the-mode-on ()
  (+ai-flymake-test--with-mode-buffers 2
    (let ((restarted nil)
          (+ai-flymake-provider 'llama))
      (cl-letf (((symbol-function 'flymake-start) (lambda (&rest _) (push (current-buffer) restarted)))
                ((symbol-function 'message) #'ignore))
        (with-temp-buffer
          (+ai/flymake-toggle-provider)))
      (ert-info ("the toggle should switch the provider")
        (should (eq 'claude +ai-flymake-provider)))
      (ert-info ("the toggle should restart the review in each buffer that has the mode, wherever it was called")
        (should (= 2 (length (seq-intersection buffers restarted))))))))

(ert-deftest +ai--flymake-callback/settles-an-aborted-request ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 1 +ai--flymake-pending 'llama)
      (funcall (+ai--flymake-callback (current-buffer) 1 report 'llama) 'abort nil)
      (ert-info ("an aborted request should clear the pending marker")
        (should (null +ai--flymake-pending)))
      (ert-info ("an aborted request should end flymake's run with no findings")
        (should (equal '((nil)) (funcall calls)))))))


;;; a request that cannot even start

(ert-deftest +ai-flymake-backend/a-request-that-cannot-start-is-reported-not-thrown ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports))
                (said nil))
      (cl-letf (((symbol-function 'gptel-request)
                 (lambda (&rest _) (user-error "ANTHROPIC_API_KEY is not set; see secrets.el.example")))
                ((symbol-function '+ai--flymake-provider) (lambda (_) (cons 'backend 'model)))
                ((symbol-function 'require) #'ignore)
                ((symbol-function 'flymake-log) #'ignore)
                ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (let ((+ai-flymake-provider 'claude))
          (+ai-flymake-backend report)))
      (ert-info ("flymake should see an empty report rather than an error that disables the backend")
        (should (equal '((nil)) (funcall calls))))
      (ert-info ("nothing should be left pending")
        (should (null +ai--flymake-pending)))
      (ert-info ("the reason should be said aloud")
        (should (equal "AI review by claude could not start: ANTHROPIC_API_KEY is not set; see secrets.el.example" said))))))

(ert-deftest +ai--flymake-restart-all/forces-flymake-past-a-disabled-backend ()
  (+ai-flymake-test--with-mode-buffers 1
    (let (args)
      (cl-letf (((symbol-function 'flymake-start) (lambda (&rest a) (setq args a))))
        (+ai--flymake-restart-all))
      (ert-info ("restarting should force flymake to run even a backend it had disabled")
        (should (equal '(nil t) args))))))

(defun +ai-flymake-test--gptel-get-backend (name)
  "gptel's lookup as it is today: an error for an unknown NAME, never nil."
  (if (equal name "Claude") 'dooms (user-error "Backend %s is not known to be defined" name)))

(ert-deftest +ai--flymake-known-backend/nil-for-a-name-gptel-rejects ()
  (cl-letf (((symbol-function 'gptel-get-backend) #'+ai-flymake-test--gptel-get-backend))
    (ert-info ("an unknown backend should come back as nil, not as an error")
      (should (null (+ai--flymake-known-backend "Claude-review"))))
    (ert-info ("a known backend should be returned")
      (should (eq 'dooms (+ai--flymake-known-backend "Claude"))))))

(ert-deftest +ai--flymake-claude/creates-the-backend-when-gptel-has-none ()
  (let (made)
    (cl-letf (((symbol-function 'gptel-get-backend) #'+ai-flymake-test--gptel-get-backend)
              ((symbol-function 'gptel-make-anthropic) (lambda (name &rest _) (setq made name) 'ours)))
      (ert-info ("a missing Claude-review backend should be created, gptel's lookup error notwithstanding")
        (should (eq 'ours (car (+ai--flymake-claude)))))
      (ert-info ("the backend should be registered under our own name")
        (should (equal "Claude-review" made))))))

(ert-deftest +ai--flymake-claude/reuses-its-backend-once-made ()
  (cl-letf (((symbol-function 'gptel-get-backend) (lambda (_) 'ours))
            ((symbol-function 'gptel-make-anthropic) (lambda (&rest _) (error "should not be remade"))))
    (ert-info ("an existing Claude-review backend should be reused")
      (should (eq 'ours (car (+ai--flymake-claude)))))))

(ert-deftest +ai--flymake-claude/uses-its-own-backend-not-doom-s ()
  (let (made)
    (cl-letf (((symbol-function 'gptel-get-backend) #'+ai-flymake-test--gptel-get-backend)
              ((symbol-function 'gptel-make-anthropic) (lambda (name &rest _) (setq made name) 'ours)))
      (ert-info ("the review should not pick up a differently keyed backend that happens to be called Claude")
        (should (eq 'ours (car (+ai--flymake-claude)))))
      (ert-info ("the backend should be registered under a name of its own")
        (should (equal "Claude-review" made))))))

;;; choosing a reviewer starts the review

(ert-deftest +ai/flymake-toggle-provider/starts-the-global-mode-when-nothing-is-reviewed ()
  (let ((started nil) (said nil)
        (+ai-flymake-provider 'llama))
    (cl-letf (((symbol-function '+ai--flymake-buffers) (lambda () nil))
              ((symbol-function '+ai-flymake-global-mode) (lambda (&optional arg) (setq started arg)))
              ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (+ai/flymake-toggle-provider))
    (ert-info ("with no buffer reviewed, the toggle should turn the global mode on")
      (should (eql 1 started)))
    (ert-info ("it should say that reviews are starting, not that zero buffers were restarted")
      (should (equal "AI review by claude: starting in every code buffer" said)))))

(ert-deftest +ai/flymake-toggle-provider/leaves-the-global-mode-alone-when-buffers-are-reviewed ()
  (+ai-flymake-test--with-mode-buffers 1
    (let ((started nil)
          (+ai-flymake-provider 'claude))
      (cl-letf (((symbol-function '+ai-flymake-global-mode) (lambda (&optional arg) (setq started arg)))
                ((symbol-function 'flymake-start) #'ignore)
                ((symbol-function 'message) #'ignore))
        (+ai/flymake-toggle-provider))
      (ert-info ("with reviewed buffers present the global mode should not be touched")
        (should (null started))))))

(ert-deftest +ai--flymake-status-text/says-what-is-on-and-who-reviews ()
  (let ((+ai-flymake-provider 'claude))
    (ert-info ("off everywhere should be said as such, with the key to turn it on")
      (should (equal "AI review by claude: off (SPC o l r to review every code buffer)"
                     (+ai--flymake-status-text nil nil nil))))
    (ert-info ("a pending review should be named")
      (should (equal "AI review by claude: on in 3 buffer(s); this one waiting on claude"
                     (+ai--flymake-status-text t 3 'claude))))
    (ert-info ("an idle buffer should be told apart from a waiting one")
      (should (equal "AI review by claude: on in 1 buffer(s); this one idle"
                     (+ai--flymake-status-text t 1 nil))))))

(ert-deftest +ai--flymake-callback/lets-a-reasoning-block-pass-and-waits-for-the-answer ()
  (+ai-flymake-test--in-buffer "x\n"
    (pcase-let ((`(,report ,calls) (+ai-flymake-test--reports)))
      (setq +ai--flymake-run 1 +ai--flymake-pending 'llama)
      (funcall (+ai--flymake-callback (current-buffer) 1 report 'llama) '(reasoning . "<think>hm</think>") nil)
      (ert-info ("a reasoning block should neither reach flymake nor end the wait")
        (should (null (funcall calls)))
        (should (eq 'llama +ai--flymake-pending))))))

;;; wrapping into free rows below

(ert-deftest +ai--flymake-wrap/breaks-at-words-within-the-width ()
  (ert-info ("text wider than the width should break between words")
    (should (equal '("aaa bbb" "ccc") (+ai--flymake-wrap "aaa bbb ccc" 7))))
  (ert-info ("text that fits should stay one line")
    (should (equal '("aaa") (+ai--flymake-wrap "aaa" 7)))))

(ert-deftest +ai--flymake-free-rows/counts-unannotated-lines-below ()
  (let ((annotated '(1 3)))
    (ert-info ("line 1 should have one free row before the annotated line 3")
      (should (= 1 (+ai--flymake-free-rows 1 annotated 5))))
    (ert-info ("line 3 should have the two remaining lines free")
      (should (= 2 (+ai--flymake-free-rows 3 annotated 5))))
    (ert-info ("the last line should have no free rows")
      (should (= 0 (+ai--flymake-free-rows 5 annotated 5))))))

(ert-deftest +ai--flymake-chunks/uses-free-rows-then-cuts-with-an-ellipsis ()
  (let ((text (propertize "aaa bbb ccc ddd" 'face 'flymake-note-echo)))
    (ert-info ("with enough free rows every chunk should appear")
      (should (equal '("aaa bbb" "ccc ddd")
                     (mapcar #'substring-no-properties (+ai--flymake-chunks text 7 3)))))
    (ert-info ("with no free row the text should be cut on its own row with an ellipsis")
      (should (equal '("aaa bb…")
                     (mapcar #'substring-no-properties (+ai--flymake-chunks text 7 0)))))
    (ert-info ("every chunk should keep the face of the summary")
      (should (seq-every-p (lambda (c) (eq 'flymake-note-echo (get-text-property 0 'face c)))
                           (+ai--flymake-chunks text 7 3))))))

(ert-deftest +ai--flymake-render/column-wraps-into-free-rows-below ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 7))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: aaa bbb ccc")))
    (let ((overlays (sort (+ai-flymake-test--overlays) (lambda (a b) (< (overlay-start a) (overlay-start b))))))
      (ert-info ("the finding should spill onto the free line below")
        (should (equal '(2 4) (mapcar #'overlay-start overlays))))
      (ert-info ("the continuation should align to the same column")
        (should (equal '(space :align-to 4)
                       (get-text-property 0 'display (overlay-get (cadr overlays) 'after-string)))))
      (ert-info ("the continuation should carry the rest of the text")
        (should (equal " ccc" (substring-no-properties (overlay-get (cadr overlays) 'after-string) 1)))))))

(ert-deftest +ai--flymake-render/column-does-not-cover-the-next-finding ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 7))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: aaa bbb ccc\n2: idea: z")))
    (let ((overlays (sort (+ai-flymake-test--overlays) (lambda (a b) (< (overlay-start a) (overlay-start b))))))
      (ert-info ("each line should keep exactly one overlay")
        (should (equal '(2 4) (mapcar #'overlay-start overlays))))
      (ert-info ("the first finding should be cut rather than spill over the second")
        (should (equal " aaa bb…" (substring-no-properties (overlay-get (car overlays) 'after-string) 1))))
      (ert-info ("the second line should show its own finding")
        (should (equal " z" (substring-no-properties (overlay-get (cadr overlays) 'after-string) 1)))))))

(ert-deftest +ai--flymake-ignored-p/keeps-secrets-from-the-reviewer ()
  (let ((+ai-flymake-skip-ignored nil))
    (pcase-dolist (`(,file . ,expected)
                   '(("/home/me/.config/doom/secrets.el" . t)
                     ("/p/.env" . t)
                     ("/p/.env.local" . t)
                     ("/home/me/.authinfo.gpg" . t)
                     ("/home/me/.netrc" . t)
                     ("/p/server.key" . t)
                     ("/p/certs/ca.pem" . t)
                     ("/p/environment.py" . nil)
                     ("/p/secrets_test.py" . nil)
                     ("/p/src/main.el" . nil)))
      (ert-info ((format "%s should%s be kept from the reviewer by name" file (if expected "" " not")))
        (should (eq expected (+ai--flymake-ignored-p file)))))))

(ert-deftest +ai--flymake-ignored-p/skips-files-git-ignores ()
  (let ((repo (file-name-as-directory (file-truename (make-temp-file "flymake-ignore" t)))))
    (unwind-protect
        (let ((default-directory repo))
          (call-process "git" nil nil nil "init" "-q")
          (with-temp-file ".gitignore" (insert "generated.py\n"))
          (with-temp-file "generated.py" (insert "x = 1\n"))
          (with-temp-file "handwritten.py" (insert "x = 1\n"))
          (let ((+ai-flymake-skip-ignored t))
            (ert-info ("a file git ignores should not be reviewed")
              (should (+ai--flymake-ignored-p (expand-file-name "generated.py" repo))))
            (ert-info ("a tracked or untracked file should still be reviewed")
              (should-not (+ai--flymake-ignored-p (expand-file-name "handwritten.py" repo)))))
          (let ((+ai-flymake-skip-ignored nil))
            (ert-info ("with the git check off, an ignored file with an innocent name should be reviewed")
              (should-not (+ai--flymake-ignored-p (expand-file-name "generated.py" repo))))))
      (delete-directory repo t))))

(ert-deftest +ai--flymake-reviewable-p/leaves-secret-files-alone ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/home/me/.config/doom/secrets.el")
    (let ((+ai-flymake-skip-ignored nil))
      (ert-info ("a secrets.el buffer should not be reviewed even though it is code")
        (should-not (+ai--flymake-reviewable-p))))))

;;; switching the review off: a buffer, or a whole project

(ert-deftest +ai--flymake-reviewable-p/stays-off-where-the-buffer-says-so ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/x.el")
    (let ((+ai-flymake-skip-ignored nil))
      (setq-local +ai-flymake-off t)
      (ert-info ("a buffer with `+ai-flymake-off' set, as a file or dir local would, should not be reviewed")
        (should-not (+ai--flymake-reviewable-p))))))

(ert-deftest +ai-flymake-off/is-safe-as-a-local-variable ()
  (ert-info ("`+ai-flymake-off' should be accepted from .dir-locals.el without a prompt")
    (should (eq #'booleanp (get '+ai-flymake-off 'safe-local-variable)))))

(ert-deftest +ai--flymake-muted-root/finds-the-muted-project-a-file-sits-in ()
  (let ((+ai--flymake-muted-roots '("/p/one" "/p/two/")))
    (ert-info ("a file under a muted root should name that root")
      (should (equal "/p/one" (+ai--flymake-muted-root "/p/one/src/a.el"))))
    (ert-info ("a root with or without its trailing slash should match the same")
      (should (equal "/p/two/" (+ai--flymake-muted-root "/p/two/b.el"))))
    (ert-info ("a sibling whose name merely starts the same should not match")
      (should-not (+ai--flymake-muted-root "/p/oneness/c.el")))
    (ert-info ("a file elsewhere should not be muted")
      (should-not (+ai--flymake-muted-root "/q/d.el")))))

(ert-deftest +ai--flymake-reviewable-p/stays-off-under-a-muted-project ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/p/one/a.el")
    (let ((+ai-flymake-skip-ignored nil)
          (+ai--flymake-muted-roots '("/p/one")))
      (ert-info ("a code buffer under a muted project should not be reviewed")
        (should-not (+ai--flymake-reviewable-p))))))

(ert-deftest +ai/flymake-mute-buffer/turns-the-review-off-and-keeps-it-off ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq buffer-file-name "/tmp/x.el")
    (let ((+ai-flymake-skip-ignored nil) (+ai-flymake-global-mode t))
      (cl-letf (((symbol-function 'flymake-start) #'ignore)
                ((symbol-function 'message) #'ignore))
        (+ai-flymake-mode 1)
        (+ai/flymake-mute-buffer)
        (ert-info ("muting should turn the mode off in the buffer")
          (should-not +ai-flymake-mode))
        (ert-info ("muting should keep the global mode from turning it back on")
          (+ai--flymake-turn-on)
          (should-not +ai-flymake-mode))
        (+ai/flymake-mute-buffer)
        (ert-info ("muting again should lift the mute and, under the global mode, review once more")
          (should +ai-flymake-mode))))))

(defmacro +ai-flymake-test--with-file-buffers (files &rest body)
  "Run BODY with a live elisp buffer per name in FILES, the mode on, bound as `buffers'."
  (declare (indent 1))
  `(let ((buffers (mapcar (lambda (file)
                            (with-current-buffer (generate-new-buffer "muted")
                              (emacs-lisp-mode)
                              (setq buffer-file-name file +ai-flymake-mode t)
                              (current-buffer)))
                          ,files)))
     (unwind-protect (progn ,@body)
       (mapc #'kill-buffer buffers))))

(ert-deftest +ai/flymake-mute-project/turns-the-review-off-under-the-root-and-back ()
  (+ai-flymake-test--with-file-buffers '("/p/one/a.el" "/p/one/src/b.el" "/p/two/c.el")
    (let ((+ai--flymake-muted-roots nil) (+ai-flymake-skip-ignored nil) (+ai-flymake-global-mode t) said)
      (cl-letf (((symbol-function '+ai--flymake-root) (lambda () "/p/one/"))
                ((symbol-function '+ai-flymake-mode)
                 (lambda (arg) (setq +ai-flymake-mode (eql arg 1))))
                ((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (with-current-buffer (car buffers) (+ai/flymake-mute-project))
        (ert-info ("muting the project should turn the mode off in every buffer under its root")
          (should (equal '(nil nil t) (mapcar (lambda (b) (buffer-local-value '+ai-flymake-mode b)) buffers))))
        (ert-info ("muting should say which project went quiet")
          (should (equal "AI review off in /p/one/" said)))
        (with-current-buffer (car buffers) (+ai/flymake-mute-project))
        (ert-info ("muting the same project again should lift the mute and review its buffers again")
          (should (equal '(t t t) (mapcar (lambda (b) (buffer-local-value '+ai-flymake-mode b)) buffers))))
        (ert-info ("the root should be gone from the muted list")
          (should (null +ai--flymake-muted-roots)))))))

(ert-deftest +ai--flymake-status-text/says-when-this-buffer-is-muted ()
  (let ((+ai-flymake-provider 'llama))
    (ert-info ("a muted buffer should be told apart from an idle one")
      (should (equal "AI review by llama: on in 2 buffer(s); this one off (SPC o l x)"
                     (+ai--flymake-status-text t 2 nil 'buffer))))
    (ert-info ("a muted project should be named")
      (should (equal "AI review by llama: on in 2 buffer(s); this one off with its project (SPC o l X)"
                     (+ai--flymake-status-text t 2 nil 'project))))))

;;; findings step aside from point and follow the code

(defun +ai-flymake-test--after-strings ()
  (mapcar (lambda (o) (cons (overlay-start o) (substring-no-properties (overlay-get o 'after-string) 1)))
          (sort (+ai-flymake-test--overlays) (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(ert-deftest +ai--flymake-nearest-free/prefers-the-row-below-then-above-then-further ()
  (ert-info ("with the row below free it should be taken")
    (should (= 3 (+ai--flymake-nearest-free 2 '(2) 5))))
  (ert-info ("with the row below taken the row above should be next")
    (should (= 1 (+ai--flymake-nearest-free 2 '(2 3) 5))))
  (ert-info ("with both neighbours taken the search should widen")
    (should (= 4 (+ai--flymake-nearest-free 2 '(1 2 3) 5))))
  (ert-info ("the last line should look above when nothing is below")
    (should (= 4 (+ai--flymake-nearest-free 5 '(5) 5))))
  (ert-info ("a buffer with no free row should give nil")
    (should (null (+ai--flymake-nearest-free 1 '(1 2) 2)))))

(ert-deftest +ai--flymake-yield/moves-the-group-on-point-s-line-pointing-back ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (let* ((groups (+ai--flymake-by-line (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x\n1: idea: y\n3: note: z")))
           (moved (+ai--flymake-yield groups 1 3)))
      (ert-info ("the group on the line should move to the row below, the other staying put")
        (should (equal '(2 3) (sort (mapcar #'car moved) #'<))))
      (ert-info ("the moved summary should point back up to its line, once")
        (should (equal "↑ x │ y" (substring-no-properties (+ai--flymake-summary (alist-get 2 moved))))))
      (ert-info ("a line with no group should leave the groups alone")
        (should (equal groups (+ai--flymake-yield groups 2 3)))))))

(ert-deftest +ai--flymake-yield/points-down-when-moved-above ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (let ((moved (+ai--flymake-yield (+ai--flymake-by-line (+ai--flymake-diagnostics (current-buffer) 'llama "2: note: x")) 2 2)))
      (ert-info ("a group moved to the row above should point down")
        (should (equal "↓ x" (substring-no-properties (+ai--flymake-summary (alist-get 1 moved)))))))))

(ert-deftest +ai--flymake-render/column-steps-aside-from-point-s-line ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (goto-char (point-min))
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 20) (+ai-flymake-yield t))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x")))
    (ert-info ("the finding of point's line should be drawn at the end of the line below, pointing up")
      (should (equal '((4 . " ↑ x")) (+ai-flymake-test--after-strings))))))

(ert-deftest +ai--flymake-render/column-never-spills-onto-point-s-line ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (goto-char (point-min)) (forward-line 1)
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 7) (+ai-flymake-yield t))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: aaa bbb ccc")))
    (ert-info ("the finding above should be cut rather than wrap onto the line point is on")
      (should (equal '((2 . " aaa bb…")) (+ai-flymake-test--after-strings))))))

(ert-deftest +ai--flymake-render/stays-put-with-yield-off-or-in-the-margin ()
  (+ai-flymake-test--in-buffer "a\nb\n"
    (goto-char (point-min))
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 20) (+ai-flymake-yield nil))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x")))
    (ert-info ("with yield off the finding should stay on its own line")
      (should (equal '((2 . " x")) (+ai-flymake-test--after-strings))))
    (+ai--flymake-clear)
    (let ((+ai-flymake-display 'margin) (+ai-flymake-yield t))
      (+ai--flymake-render (+ai--flymake-diagnostics (current-buffer) 'llama "1: note: x")))
    (ert-info ("the margin never blocks typing, so its finding should stay on its line")
      (should (= 1 (overlay-start (car (+ai-flymake-test--overlays))))))))

(ert-deftest +ai--flymake-native-eol-p/is-off-while-findings-yield ()
  (let ((+ai-flymake-yield t))
    (ert-info ("flymake's own end-of-line display cannot step aside, so ours should be used while yielding")
      (should-not (+ai--flymake-native-eol-p)))))

(defun +ai-flymake-test--report-through-flymake (response)
  "Have flymake carry our findings for RESPONSE in this buffer, as a real run would."
  (let ((diagnostics (+ai--flymake-diagnostics (current-buffer) 'llama response)))
    (cl-letf (((symbol-function '+ai-flymake-backend)
               (lambda (report-fn &rest _) (funcall report-fn diagnostics))))
      (setq-local flymake-diagnostic-functions '(+ai-flymake-backend))
      (flymake-mode 1)
      (flymake-start nil t))))

(ert-deftest +ai--flymake-live-findings/follows-flymake-s-underline-through-edits ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (+ai-flymake-test--report-through-flymake "2: note: x")
    (ert-info ("before any edit the finding should sit on the line the model named")
      (should (equal '(2) (mapcar #'car (+ai--flymake-live-findings)))))
    (goto-char (point-min))
    (insert "new\n")
    (ert-info ("a line inserted above should move the finding down with its code")
      (should (equal '(3) (mapcar #'car (+ai--flymake-live-findings)))))
    (ert-info ("the diagnostic itself should be the one flymake holds")
      (should (equal "llama: x" (flymake-diagnostic-text (cdar (+ai--flymake-live-findings))))))))

(ert-deftest +ai--flymake-follow/places-findings-again-after-an-edit ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 20) (+ai-flymake-yield nil))
      (+ai-flymake-test--report-through-flymake "2: note: x")
      (+ai--flymake-place (+ai--flymake-live-findings))
      (goto-char (point-min))
      (insert "new\n")
      (+ai--flymake-mark-stale)
      (+ai--flymake-follow)
      (ert-info ("after an edit the finding should be drawn at the end of the line its code moved to")
        (should (equal '((8 . " x")) (+ai-flymake-test--after-strings))))
      (ert-info ("placing should clear the stale mark")
        (should-not +ai--flymake-stale)))))

(ert-deftest +ai--flymake-follow/steps-aside-when-point-reaches-the-line ()
  (+ai-flymake-test--in-buffer "a\nb\nc\n"
    (let ((+ai-flymake-display 'column) (+ai-flymake-column 4) (+ai-flymake-wrap-width 20) (+ai-flymake-yield t))
      (+ai-flymake-test--report-through-flymake "2: note: x")
      (goto-char (point-min))
      (+ai--flymake-place (+ai--flymake-live-findings))
      (ert-info ("with point elsewhere the finding should sit on its own line")
        (should (equal '((4 . " x")) (+ai-flymake-test--after-strings))))
      (forward-line 1)
      (+ai--flymake-follow)
      (ert-info ("once point is on the line the finding should have moved to the row below")
        (should (equal '((6 . " ↑ x")) (+ai-flymake-test--after-strings))))
      (+ai--flymake-follow)
      (ert-info ("staying on the line should not redraw")
        (should (equal '((6 . " ↑ x")) (+ai-flymake-test--after-strings)))))))

(ert-deftest +ai-flymake-mode/watches-edits-and-point-while-on ()
  (+ai-flymake-test--in-buffer "a\n"
    (cl-letf (((symbol-function 'flymake-start) #'ignore)
              ((symbol-function 'flymake-mode) #'ignore))
      (+ai-flymake-mode 1)
      (ert-info ("the mode should watch edits in this buffer")
        (should (memq #'+ai--flymake-mark-stale after-change-functions)))
      (ert-info ("the mode should place findings again after each command")
        (should (memq #'+ai--flymake-follow post-command-hook)))
      (+ai-flymake-mode -1)
      (ert-info ("turning the mode off should stop watching")
        (should-not (memq #'+ai--flymake-follow post-command-hook))
        (should-not (memq #'+ai--flymake-mark-stale after-change-functions))))))
