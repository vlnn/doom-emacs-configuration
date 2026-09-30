;;; my/lsp/config.el -*- lexical-binding: t; -*-

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

(after! (jsonian flycheck) (jsonian-enable-flycheck))
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-auto-register-remote-workspace-folders nil
        lsp-enable-suggest-server-download nil
        lsp-copilot-enabled nil
        lsp-session-folders-blocklist
        (mapcar #'expand-file-name '("~/Downloads" "~/" "/tmp"))))
