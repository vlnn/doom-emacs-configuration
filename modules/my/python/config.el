;;; my/python/config.el -*- lexical-binding: t; -*-

(defun +python--run-from-project-root (orig-fun &rest args)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (apply orig-fun args)))

(defvar +python--last-window nil
  "Window the user came from when toggling the Python REPL.")

(defun +python/toggle-repl ()
  (interactive)
  (if (eq major-mode 'inferior-python-mode)
      (when (window-live-p +python--last-window)
        (select-window +python--last-window))
    (setq +python--last-window (selected-window))
    (python-shell-switch-to-shell)))

(after! python
  (setq python-shell-prompt-detect-failure-warning nil
        python-shell-interpreter "uv"
        python-shell-interpreter-args "run python -i")

  (advice-add 'run-python :around #'+python--run-from-project-root)

  (map! :leader :desc "Toggle Python REPL" "o z" #'+python/toggle-repl)

  (map! :map python-mode-map
        :leader
        :prefix ("r" . "refactor")
        :desc "Extract variable" "v" #'lsp-extract-variable
        :desc "Extract method"   "m" #'lsp-extract-method
        :desc "Inline variable"  "i" #'lsp-inline-variable
        :desc "Rename symbol"    "r" #'lsp-rename
        :desc "Organize imports" "o" #'lsp-organize-imports))

(after! lsp-pylsp
  (setq lsp-pylsp-plugins-rope-completion-enabled t
        lsp-pylsp-plugins-rope-autoimport-enabled t
        lsp-pylsp-rename-backend 'rope
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-black-enabled t
        lsp-pylsp-plugins-isort-enabled t))

(load! "dape")
