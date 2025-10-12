;;; python.el -*- lexical-binding: t; -*-

(use-package! eglot
  :config
  (add-to-list 'eglot-server-programs
               '(python-mode . ("ty" "server")))
  (add-to-list 'eglot-server-programs
               `(python-mode . ,(eglot-alternatives
                                 '(("basedpyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives
                                    '(("basedpyright-langserver" "--stdio"))))))

(after! python
  (require 'dape)
  (setq projectile-create-missing-test-files t
        projectile-project-test-dir "tests"))

(defun my/format-buffer-with-ruff ()
  "Format current buffer using uv run ruff format."
  (when (eq major-mode 'python-mode)
    (let* ((file (buffer-file-name))
           (command (format "uv run ruff format %s" (shell-quote-argument file))))
      (shell-command command)
      (revert-buffer t t t))))

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'my/format-buffer-with-ruff nil t)))

(use-package! pet
  :config
  (add-hook 'python-base-mode-hook 'pet-mode -10))

