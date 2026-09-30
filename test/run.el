;;; test/run.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -l test/run.el

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (setq default-directory root)
  (add-to-list 'load-path (expand-file-name "test" root))
  (require 'ert)
  (require 'doom-stubs)
  (dolist (file '("config.el" "snippets.el" "dape.el" "ai.el"))
    (load (expand-file-name file root) nil t))
  (dolist (test (directory-files (expand-file-name "test" root) t "-test\\.el\\'"))
    (load test nil t))
  (ert-run-tests-batch-and-exit))
