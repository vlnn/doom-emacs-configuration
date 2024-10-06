;;; python.el -*- lexical-binding: t; -*-

(after! python
  (map! :map python-mode-map
        :localleader
        :prefix ("e" . "eval")
        :desc "statement"  "e" #'python-shell-send-statement
        :desc "function"  "f" #'python-shell-send-defun
        :desc "file"  "F" #'python-shell-send-file
        :desc "region"  "r" #'python-shell-send-region))

(after! python
  (map! :map python-mode-map
        :localleader
        :prefix ("r" . "repl")
        :desc "REPL"  "r" #'+python/open-ipython-repl))

(use-package! pipenv
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  :config
  (map! :map python-mode-map
        :localleader
        :prefix ("p" . "pipenv")
        :desc "activate"    "a" #'pipenv-activate
        :desc "deactivate"  "d" #'pipenv-deactivate
        :desc "install"     "i" #'pipenv-install
        :desc "lock"        "l" #'pipenv-lock
        :desc "open module" "o" #'pipenv-open
        :desc "run"         "r" #'pipenv-run
        :desc "shell"       "s" #'pipenv-shell
        :desc "uninstall"   "u" #'pipenv-uninstall))


(comment (setq treesit-language-source-alist
               '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                 (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                 (html "https://github.com/tree-sitter/tree-sitter-html")
                 (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                 (json "https://github.com/tree-sitter/tree-sitter-json")
                 (make "https://github.com/alemuller/tree-sitter-make")
                 (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                 (python "https://github.com/tree-sitter/tree-sitter-python")
                 (toml "https://github.com/tree-sitter/tree-sitter-toml")
                 (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(comment (after! python
           (setq python-shell-interpreter "ipython"
                 python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True")
           (setq python-ts-mode-hook python-mode-hook)
           (add-hook 'python-mode-hook
                     (lambda ()
                       (setq-local lsp-ruff-lsp-python-path python-shell-interpreter)))
           (setq treesit-font-lock-level 4)
           (add-hook 'python-ts-mode-hook 'python-coverage-overlay-mode)
           (add-hook 'python-mode-hook 'python-coverage-overlay-mode)
           (setq lsp-pylsp-plugins-ruff-config "~/.config/ruff/ruff.toml")
           (add-to-list 'flycheck-python-ruff-config "~/.config/ruff/ruff.toml")
           (add-hook 'python-ts-mode-hook 'ruff-format-on-save-mode)))



(comment (use-package! eglot
           :hook ((python-ts-mode . eglot-ensure)
                  (python-mode . eglot-ensure)
                  (rust-mode . eglot-ensure)
                  (js-mode . eglot-ensure)
                  (java-mode . eglot-ensure)
                  (c-mode . eglot-ensure)
                  (c++-mode . eglot-ensure))
           :config
           (add-to-list 'eglot-server-programs
                        `((python-ts-mode) .
                          ,(eglot-alternatives '("ruff server --preview" "jedi-language-server" ("poetry" "run" "pyright-langserver" "--stdio")  ("pyright-langserver" "--stdio")))))
           (setq eglot-auto-display-help-buffer nil
                 eglot-send-changes-idle-time 0.5
                 eglot-connect-timeout 20)))

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
    :modes python-ts-mode)

  (add-to-list 'flycheck-checkers 'my-python-ruff))


(comment (map! :map python-ts-mode-map
               (:localleader
                (:prefix ("r" . "REPL send")
                 :desc "Buffer"   "b" #'python-shell-send-buffer
                 :desc "Function" "f" #'python-shell-send-defun
                 :desc "Region"   "r" #'python-shell-send-region))))

(defun my/run-coverage-and-refresh ()
  "Run coverage and refresh overlays."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "poetry run coverage run -m pytest && coverage xml")
    (python-coverage-overlay-refresh)))

(map! :leader
      :desc "Run coverage and refresh"
      "p c" #'my/run-coverage-and-refresh)

(after! clojure-mode
  (setq dash-docs-docsets '("Clojure")))

(after! python-mode
  (setq dash-docs-docsets '("Python")))

