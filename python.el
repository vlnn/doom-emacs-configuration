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
  (setq python-shell-interpreter "poetry")
  python-shell-interpreter-args "run python"
  (setq python-ts-mode-hook python-mode-hook)
  (setq-local lsp-ruff-lsp-python-path python-shell-interpreter)
  (setq treesit-font-lock-level 3)
  (add-hook 'python-ts-mode-hook 'python-coverage-overlay-mode)
  (add-hook 'python-mode-hook 'python-coverage-overlay-mode)
  (setq lsp-pylsp-plugins-ruff-config "~/.config/ruff/ruff.toml")
  (setq flycheck-python-ruff-executable "ruff")
  (setq-hook! 'python-mode-hook +format-with 'ruff)
  (add-to-list 'flycheck-python-ruff-config "~/.config/ruff/ruff.toml")
  (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode))


(after! flycheck
  ;; Add support for Ruff Python linter.
  (defvar my-ruff-global-config "~/.config/ruff/ruff.toml"
    "Path of global Ruff configuration file.")

  (flycheck-define-checker my-python-ruff
    "A Python syntax and style checker using the ruff utility."
    :command ("ruff"
              "check"
              "--output-format=full"
              (eval (format "--config=%s" my-ruff-global-config))
              (eval (when buffer-file-name
                      (concat "--stdin-filename=" buffer-file-name)))
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let ((errors (flycheck-sanitize-errors errors)))
                      (seq-map #'flycheck-flake8-fix-error-level errors)))
    :error-patterns
    ((warning line-start
              (file-name) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit)) " "
              (message (one-or-more not-newline))
              line-end))
    :modes (python-ts-mode python-mode))

  (add-to-list 'flycheck-checkers 'my-python-ruff)
  (add-to-list 'flycheck-enabled-checkers 'my-python-ruff))

(after! python-mode
  (add-hook 'python-mode-hook
            (lambda ()
              (when (buffer-file-name)
                (setq-local flycheck-checker 'my-python-ruff)))))

(defun my/run-coverage-and-refresh ()
  "Run coverage and refresh overlays."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "poetry run coverage run -m pytest && coverage xml")
    (python-coverage-overlay-refresh)))

(map! :leader
      :desc "Run coverage and refresh"
      "p c" #'my/run-coverage-and-refresh)


(after! python-mode
  (setq dash-docs-docsets '("Python")))
