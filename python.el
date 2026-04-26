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
