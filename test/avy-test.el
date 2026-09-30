;;; test/avy-test.el -*- lexical-binding: t; -*-

(unless (fboundp 'clojure-mode)
  (define-derived-mode clojure-mode prog-mode "clojure-stub"))

(defmacro +avy-test--with-eval-spies (mode &rest body)
  "Run BODY in a buffer whose major mode is MODE, recording which evaluator fires in `calls'."
  (declare (indent 1))
  `(let ((calls nil))
     (cl-letf (((symbol-function 'cider-eval-last-sexp)     (lambda () (push 'cider-sexp calls)))
               ((symbol-function 'cider-eval-defun-at-point) (lambda () (push 'cider-defun calls)))
               ((symbol-function '+eval/region)              (lambda (b e) (push (list 'region b e) calls))))
       (with-temp-buffer
         (funcall ,mode)
         (insert "(defun f () (+ 1 2))")
         ,@body
         (nreverse calls)))))

(ert-deftest +avy--eval-sexp-before-point/uses-cider-in-clojure ()
  (ert-info ("clojure buffers should evaluate through cider")
    (should (equal '(cider-sexp)
                   (+avy-test--with-eval-spies #'clojure-mode
                     (+avy--eval-sexp-before-point))))))

(ert-deftest +avy--eval-sexp-before-point/uses-eval-region-elsewhere ()
  (ert-info ("non-clojure buffers should evaluate the sexp before point via +eval/region")
    (should (equal '((region 13 20))
                   (+avy-test--with-eval-spies #'emacs-lisp-mode
                     (goto-char 20)
                     (+avy--eval-sexp-before-point))))))

(ert-deftest +avy--eval-defun-at-point/uses-eval-region-elsewhere ()
  (ert-info ("non-clojure buffers should evaluate the whole defun via +eval/region")
    (should (equal '((region 1 21))
                   (+avy-test--with-eval-spies #'emacs-lisp-mode
                     (goto-char 5)
                     (+avy--eval-defun-at-point))))))
