;;; test/harness.el -*- lexical-binding: t; -*-
;; Shared by test/run.el and test/cold.el: which config files load, and how the suite runs.

(require 'ert)

(defconst +test-unloadable
  '("modules/my/editing/keychords.el")
  "Config files that call a package at top level and so cannot load under the stubs.")

(defun +test--loadable-p (file root)
  (not (or (string-suffix-p "packages.el" file)
           (member (file-relative-name file root) +test-unloadable))))

(defun +test--module-files (root)
  (sort (directory-files-recursively (expand-file-name "modules" root) "\\.el\\'") #'string<))

(defun +test--prepare (root)
  (setq default-directory root)
  (add-to-list 'load-path (expand-file-name "test" root))
  ;; Stubbing a primitive (message, read-string, call-process) would native-compile a trampoline
  ;; for it; tests call stubs from interpreted code only, so they need no compiler.
  (setq native-comp-enable-subr-trampolines nil)
  (require 'org)
  (require 'doom-stubs))

(defun +test--load-modules (root loadable-p)
  (dolist (file (+test--module-files root))
    (when (funcall loadable-p file root)
      (load file nil t))))

(defun +test--defined (function)
  "Load the autoload file defining FUNCTION, so a test may stub what that file defines."
  (autoload-do-load (symbol-function function) function))

(defun +test--run (root)
  (dolist (test (directory-files (expand-file-name "test" root) t "-test\\.el\\'"))
    (load test nil t))
  (ert-run-tests-batch-and-exit))

(provide 'harness)
