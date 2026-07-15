;;; python.el -*- lexical-binding: t; -*-

(after! python
  (setq python-shell-prompt-detect-failure-warning nil
        python-shell-interpreter "uv"
        python-shell-interpreter-args "run python -i")

  (defun +python--run-from-project-root (orig-fun &rest args)
    (let ((default-directory (or (projectile-project-root) default-directory)))
      (apply orig-fun args)))

  (advice-add 'run-python :around #'+python--run-from-project-root)

  (defvar +python--last-window nil
    "Window the user came from when toggling the Python REPL.")

  (defun +python/toggle-repl ()
    (interactive)
    (if (eq major-mode 'inferior-python-mode)
        (when (and +python--last-window (window-live-p +python--last-window))
          (select-window +python--last-window))
      (setq +python--last-window (selected-window))
      (python-shell-switch-to-shell)))

  (map! :leader :desc "Toggle Python REPL" "o z" #'+python/toggle-repl))

(after! lsp-mode
  (setq lsp-pylsp-plugins-rope-completion-enabled t
        lsp-pylsp-plugins-rope-autoimport-enabled t
        lsp-pylsp-rename-backend 'rope
        lsp-pylsp-plugins-pydocstyle-enabled nil  ; Optional: disable if annoying
        lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-black-enabled t
        lsp-pylsp-plugins-isort-enabled t))

;; Ensure pylsp is used for python-mode, not basedpyright
(after! lsp-mode
  (add-hook! 'python-mode-hook #'lsp-deferred)
  (setq lsp-enabled-clients '(pylsp)))


(after! lsp-mode
  (map! :map python-mode-map
        :leader
        :prefix ("r" . "refactor")
        :desc "Extract variable" "v" #'lsp-extract-variable
        :desc "Extract method" "m" #'lsp-extract-method
        :desc "Inline variable" "i" #'lsp-inline-variable
        :desc "Rename symbol" "r" #'lsp-rename
        :desc "Organize imports" "o" #'lsp-organize-imports))
