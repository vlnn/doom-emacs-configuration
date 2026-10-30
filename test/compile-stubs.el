;;; test/compile-stubs.el -*- lexical-binding: t; -*-
;; Doom's macro surface for `bin/compile-check': the bodies of `after!',
;; `use-package!', `add-hook!' and `defadvice!' are kept, so the compiler sees
;; them; the DSLs (`map!', popup and handler rules) expand to nil. Bodies sit
;; under `when', not at top level, so a `require' in them is compiled, not run.

(require 'doom-stubs)

(defmacro after! (_features &rest body)
  `(when t ,@body))

(defun +compile-stubs--plist-bodies (plist keys)
  "The forms under each of KEYS in the use-package PLIST, in order."
  (let (current bodies)
    (dolist (form plist (nreverse bodies))
      (cond ((keywordp form) (setq current form))
            ((memq current keys) (push form bodies))))))

(defmacro use-package! (_name &rest plist)
  `(when t ,@(+compile-stubs--plist-bodies plist '(:init :config))))

(defmacro add-hook! (_hooks &rest rest)
  `(when t ,@(seq-remove #'keywordp rest)))

(defun +compile-stubs--without-keywords (forms)
  (let (result)
    (while forms
      (if (keywordp (car forms))
          (setq forms (cddr forms))
        (push (pop forms) result)))
    (nreverse result)))

(defmacro defadvice! (symbol args &rest body)
  `(defun ,symbol ,args ,@(+compile-stubs--without-keywords body)))

(defmacro modulep! (&rest _) t)

(provide 'compile-stubs)
