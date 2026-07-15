;;; lsp.el -*- lexical-binding: t; -*-

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(after! (jsonian flycheck) (jsonian-enable-flycheck))
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(after! lsp-mode
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-auto-register-remote-workspace-folders nil
        lsp-enable-suggest-server-download nil
        lsp-copilot-enabled nil
        ;; Keep basedpyright for other languages, but override for Python
        lsp-pyright-langserver-command "basedpyright"
        lsp-session-folders-blocklist
        (mapcar #'expand-file-name
                '("~/Downloads" "~/" "/tmp" )))

  ;; Force pylsp for Python mode (needed for refactoring)
  (add-to-list 'lsp-enabled-clients '(python-mode . pylsp))

  (advice-add 'lsp :before
              (lambda (&rest _)
                (setq lsp-session-folders (list (projectile-project-root))))))
