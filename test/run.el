;;; test/run.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -l test/run.el   (or bin/test)

(defconst +test-unloadable
  '("modules/my/editing/keychords.el")
  "Config files that call a package at top level and so cannot load under the stubs.")

(defun +test--loadable-p (file root)
  (not (or (string-suffix-p "packages.el" file)
           (member (file-relative-name file root) +test-unloadable))))

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (setq default-directory root)
  (add-to-list 'load-path (expand-file-name "test" root))
  (require 'ert)
  ;; Stubbing a primitive (message, read-string, call-process) would native-compile a trampoline
  ;; for it; tests call stubs from interpreted code only, so they need no compiler.
  (setq native-comp-enable-subr-trampolines nil)
  (require 'org)
  (require 'doom-stubs)
  (dolist (file (sort (directory-files-recursively (expand-file-name "modules" root) "\\.el\\'") #'string<))
    (when (+test--loadable-p file root)
      (load file nil t)))
  (dolist (test (directory-files (expand-file-name "test" root) t "-test\\.el\\'"))
    (load test nil t))
  (ert-run-tests-batch-and-exit))
