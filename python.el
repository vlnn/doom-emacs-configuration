;;; python.el -*- lexical-binding: t; -*-
(after! python
  (require 'dape)
  (setq projectile-create-missing-test-files t
        projectile-project-test-dir "tests"))

(comment
        ;; Minimal LSP config - just pylsp
        (with-eval-after-load 'lsp-mode)
        (setq lsp-warn-no-matched-clients nil
                      lsp-response-timeout 10  ; Fail fast
                      lsp-log-io t)  ; Enable logging to see what's happening

        ;; Disable ruff completely for now
        (setq lsp-disabled-clients '(ruff-lsp))

        ;; Just use pylsp
        (setq lsp-python-server 'pylsp))

(use-package! pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

