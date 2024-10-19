;;; python.el -*- lexical-binding: t; -*-

(map! :map (python-mode-map python-ts-mode-map)
      :localleader
      :prefix ("e" . "eval")
      :desc "statement"  "e" #'python-shell-send-statement
      :desc "function"  "f" #'python-shell-send-defun
      :desc "file"  "F" #'python-shell-send-file
      :desc "region"  "r" #'python-shell-send-region)

(map! :map (python-ts-mode-map python-mode-map)
      :localleader
      :prefix ("r" . "repl")
      :desc "REPL"  "r" #'+python/open-ipython-repl
      :desc "Buffer"   "b" #'python-shell-send-buffer
      :desc "Function" "f" #'python-shell-send-defun
      :desc "Region"   "R" #'python-shell-send-region)

(after! python
  (setq dash-docs-docsets '("Python"))
  (setq python-shell-interpreter "poetry"
        python-shell-interpreter-args "run python")
  (setq python-ts-mode-hook python-mode-hook)
  (setq-local lsp-ruff-lsp-python-path python-shell-interpreter)
  (setq treesit-font-lock-level 4)
  (add-hook 'python-ts-mode-hook 'python-coverage-overlay-mode)
  (add-hook 'python-mode-hook 'python-coverage-overlay-mode)
  (add-hook 'python-mode-hook 'lsp)
  (add-hook 'python-ts-mode-hook 'lsp)
  (setq flycheck-python-ruff-executable "ruff")
  (setq-hook! 'python-mode-hook +format-with 'ruff)
  (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))


(defun my/run-coverage-and-refresh ()
  "Run coverage and refresh overlays."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "poetry run coverage run -m pytest && coverage xml")
    (python-coverage-overlay-refresh)))

(map! :leader
      :desc "Run coverage and refresh"
      "p c" #'my/run-coverage-and-refresh)


(after! (:and python-mode apheleia-mode)
  (add-to-list 'apheleia-mode-alist '(python-mode . '(ruff-isort ruff)))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . '(ruff-isort ruff))))
