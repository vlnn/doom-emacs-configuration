;;; test/ai-implement-test.el -*- lexical-binding: t; -*-

(defmacro +ai-implement-test--in-buffer (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,text)
     ,@body))

(defun +ai-implement-test--select (beg end)
  (set-mark beg)
  (goto-char end)
  (activate-mark))

;;; context

(ert-deftest +ai--implement-context/whole-buffer-by-default ()
  (+ai-implement-test--in-buffer "(defun f ())\n(defun g ())\n"
    (goto-char 5)
    (ert-info ("without a region the whole buffer should be the context")
      (should (equal "(defun f ())\n(defun g ())\n" (+ai--implement-context))))))

(ert-deftest +ai--implement-context/region-wins ()
  (+ai-implement-test--in-buffer "(defun f ())\n(defun g ())\n"
    (+ai-implement-test--select 1 13)
    (let ((transient-mark-mode t))
      (ert-info ("an active region should be the context on its own")
        (should (equal "(defun f ())" (+ai--implement-context)))))))

(ert-deftest +ai--implement-prompt/wraps-context-and-request ()
  (ert-info ("the prompt should fence the context and state the request after it")
    (should (equal "<context>\n(defun f ())\n</context>\n\nImplement: a g that calls f twice"
                   (+ai--implement-prompt "(defun f ())" "a g that calls f twice")))))

;;; the reply

(ert-deftest +ai--implement-code/strips-fences-and-prose-around-them ()
  (ert-info ("a fenced reply should be reduced to what is inside the fence")
    (should (equal "(defun g () (f) (f))"
                   (+ai--implement-code "Here you go:\n```elisp\n(defun g () (f) (f))\n```\nEnjoy."))))
  (ert-info ("a bare reply should pass through, trimmed")
    (should (equal "(defun g ())" (+ai--implement-code "\n(defun g ())\n\n")))))

(ert-deftest +ai--implement-insert/lands-at-the-marker-on-lines-of-its-own ()
  (+ai-implement-test--in-buffer "(defun f ())\n(defun h ())\n"
    (let ((at (copy-marker 14)))
      (+ai--implement-insert at "(defun g ())")
      (ert-info ("the code should be inserted where the marker was, the rest untouched")
        (should (equal "(defun f ())\n(defun g ())\n(defun h ())\n" (buffer-string)))))))

(ert-deftest +ai--implement-insert/breaks-the-line-when-point-was-mid-line ()
  (+ai-implement-test--in-buffer "(defun f ())\n"
    (let ((at (copy-marker 7)))
      (+ai--implement-insert at "(x)")
      (ert-info ("code should never be glued into the middle of a line")
        (should (equal "(defun\n(x)\n f ())\n" (buffer-string)))))))

(ert-deftest +ai--implement-insert/keeps-the-region-and-reports-where-it-went ()
  (+ai-implement-test--in-buffer "(defun f ())\n(defun h ())\n"
    (+ai-implement-test--select 1 13)
    (let ((transient-mark-mode t) (at (copy-marker 14)))
      (ert-info ("inserting should say which line the code starts on")
        (should (equal 2 (+ai--implement-insert at "(defun g ())"))))
      (ert-info ("the region that served as context should still be there")
        (should (equal "(defun f ())" (buffer-substring (region-beginning) (region-end))))))))

(ert-deftest +ai--implement-callback/inserts-a-reply-and-says-so ()
  (+ai-implement-test--in-buffer "a\n"
    (let ((at (copy-marker 3)) said)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (funcall (+ai--implement-callback at) "```\n(b)\n(c)\n```" nil))
      (ert-info ("a reply should be written into the buffer at the marker")
        (should (equal "a\n(b)\n(c)\n" (buffer-string))))
      (ert-info ("the outcome should say how much landed and where")
        (should (equal "Implemented: 2 lines at line 2" said))))))

(ert-deftest +ai--implement-callback/reports-a-failure-and-writes-nothing ()
  (+ai-implement-test--in-buffer "a\n"
    (let ((at (copy-marker 3)) said)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (funcall (+ai--implement-callback at) nil '(:status "500")))
      (ert-info ("a failed request should leave the buffer alone")
        (should (equal "a\n" (buffer-string))))
      (ert-info ("the failure should be said with its status")
        (should (equal "Implement failed: 500" said))))))

(ert-deftest +ai--implement-callback/ignores-a-killed-buffer ()
  (let ((buffer (generate-new-buffer "gone")) at)
    (with-current-buffer buffer (insert "a\n") (setq at (copy-marker 3)))
    (kill-buffer buffer)
    (ert-info ("a reply for a buffer that is gone should be dropped, not signalled")
      (should (null (funcall (+ai--implement-callback at) "(b)" nil))))))

;;; the command

(ert-deftest +ai/implement/loads-gptel-before-asking ()
  (+ai-implement-test--in-buffer "x\n"
    (let (steps)
      (cl-letf (((symbol-function 'require) (lambda (feature &rest _) (push feature steps)))
                ((symbol-function 'gptel-request) (lambda (&rest _) (push 'asked steps)))
                ((symbol-function 'message) #'ignore))
        (+ai/implement "y"))
      (ert-info ("gptel should be loaded before its request function is called, since only loading defines it")
        (should (equal '(gptel asked) (reverse steps)))))))

(ert-deftest +ai/implement/sends-the-context-and-request-to-gptel ()
  (+ai-implement-test--in-buffer "(defun f ())\n"
    (let (sent)
      (cl-letf (((symbol-function 'require) #'ignore)
                ((symbol-function 'gptel-request) (lambda (prompt &rest args) (setq sent (cons prompt args))))
                ((symbol-function 'message) #'ignore))
        (+ai/implement "a g"))
      (ert-info ("the request should carry the context and the wish")
        (should (equal "<context>\n(defun f ())\n\n</context>\n\nImplement: a g" (car sent))))
      (ert-info ("the request should use the implement directive")
        (should (eq +ai-implement-directive (plist-get (cdr sent) :system)))))))
