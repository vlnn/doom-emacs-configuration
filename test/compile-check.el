;;; test/compile-check.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -Q -l test/compile-check.el   (or bin/compile-check)

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (setq default-directory root)
  (add-to-list 'load-path (expand-file-name "test" root))
  (require (quote compile-stubs))
  (require 'bytecomp)
  (require 'json)
  ;; let-bound dynamically in the config; their packages are not loaded here
  (defvar gptel-model nil)
  (defvar vertico-sort-function nil)
  ;; free-vars and unresolved stay off: a config is mostly setq of package variables
  ;; and calls into packages that are not loaded here. Everything else fails the run.
  (setq byte-compile-warnings '(not docstrings free-vars unresolved noruntime)
        byte-compile-error-on-warn t
        byte-compile-dest-file-function (lambda (_) (make-temp-file "compile-check" nil ".elc")))
  (let ((files (append (directory-files root t "^[^.].*\\.el\\'")
                       (directory-files-recursively (expand-file-name "modules" root) "\\.el\\'")))
        (errors 0))
    (dolist (file files)
      (unless (string-match-p "/\\(secrets\\|custom\\|init\\|packages\\)\\.el\\'" file)
        (message "compiling %s" (file-relative-name file root))
        (unless (byte-compile-file file)
          (setq errors (1+ errors)))))
    (message "%d file(s) failed to compile" errors)
    (kill-emacs (if (zerop errors) 0 1))))
