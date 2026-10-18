;;; test/run.el -*- lexical-binding: t; -*-
;; Usage: emacs --batch -l test/run.el   (or bin/test)

(let ((root (file-name-directory (directory-file-name (file-name-directory load-file-name)))))
  (setq default-directory root)
  (add-to-list 'load-path (expand-file-name "test" root))
  (require 'ert)
  ;; Stubbing a primitive (message, read-string, call-process) would native-compile a trampoline
  ;; for it; tests call stubs from interpreted code only, so they need no compiler.
  (setq native-comp-enable-subr-trampolines nil)
  (require 'org)
  (require 'doom-stubs)
  (dolist (file '("modules/my/editing/avy-functions.el"
                  "modules/my/snippets/config.el"
                  "modules/my/python/dape.el"
                  "modules/my/ai/config.el"
                  "modules/my/ai/annotate.el"
                  "modules/my/ai/flymake.el"
                  "modules/my/lua/repl.el"
                  "modules/my/lua/busted.el"
                  "modules/my/lua/love.el"
                  "modules/my/ai/jail.el"
                  "modules/my/ai/trust.el"
                  "modules/my/vc/config.el"
                  "modules/my/tasks/config.el"))
    (load (expand-file-name file root) nil t))
  (dolist (test (directory-files (expand-file-name "test" root) t "-test\\.el\\'"))
    (load test nil t))
  (ert-run-tests-batch-and-exit))
