;;; python.el -*- lexical-binding: t; -*-

(use-package! lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-deferred))))  ; fixed typo: lsp-defered -> lsp-deferred

;; Add Rope for refactoring capabilities
(use-package! lsp-python-ms
  :after lsp-mode
  :init
  (setq lsp-python-ms-extra-paths nil
        lsp-python-ms-python-executable-cmd "python"
        lsp-python-ms-auto-install-server t))

;; Configure rope-specific settings
(after! lsp-mode
  (setq lsp-rope-extension-modules nil
        lsp-rope-completion-enabled t
        lsp-rope-organize-imports-on-save nil))

;; Enable multiple servers to run simultaneously
(after! lsp-mode
  (setq lsp-enabled-clients '(pyright ruff ms-python)))

(after! python
  (advice-add '+python/open-repl :around
              (lambda (orig-fun &rest args)
                (let ((default-directory (or poetry-project-root default-directory)))
                  (apply orig-fun args))))
  (poetry-tracking-mode))

(after! lsp-mode
  (setq lsp-diagnostic-clean-after-change nil
        lsp-idle-delay 0.5
        lsp-signature-function nil
        lsp-eldoc-render-all t
        lsp-enable-symbol-highlighting t
        lsp-completion-provider :capf
        lsp-completion-show-detail t
        lsp-headerline-breadcrumb-enable t
        ;; Enable more code actions
        lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-file-watchers t
        ;; Ensure code action kinds are enabled
        lsp-enable-snippet t))

;; Make sure to enable code actions in lsp-ui
(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-symbol nil
        ;; Enable code actions in the sideline
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))

;; Specifically add keybindings for code actions
(map! :after lsp-mode
      :map lsp-mode-map
      :leader
      (:prefix ("c" . "code")
       :desc "Code action" "a" #'lsp-execute-code-action
       :desc "Rename" "r" #'lsp-rename
       :desc "Format buffer" "f" #'lsp-format-buffer
       :desc "Format region" "r" #'lsp-format-region
       :desc "Extract method" "e" (lambda () (interactive) 
                                    (lsp-execute-code-action '(("refactor.extract.method"))))))

(map! :after python
      :map (python-mode-map python-ts-mode-map)
      :localleader
      (:prefix ("e" . "eval")
       :desc "eval statement"  "e" #'python-shell-send-statement
       :desc "eval function"  "f" #'python-shell-send-defun
       :desc "eval file"      "F" #'python-shell-send-file
       :desc "eval buffer"    "r" #'python-shell-send-buffer
       :desc "eval region"    "r" #'python-shell-send-region)
      (:prefix ("p" . "pipenv")
       :desc "run"       "r" #'pipenv-run)
      ;; Add refactoring prefix
      (:prefix ("r" . "refactor")
       :desc "extract method" "m" (lambda () (interactive)
                                    (lsp-execute-code-action '(("refactor.extract.method"))))
       :desc "extract variable" "v" (lambda () (interactive)
                                      (lsp-execute-code-action '(("refactor.extract.variable"))))
       :desc "inline variable" "i" (lambda () (interactive)
                                     (lsp-execute-code-action '(("refactor.inline.variable"))))
       :desc "organize imports" "o" #'lsp-organize-imports))

(set-popup-rules!
  '(("^ \\*" :slot -1)))

(after! dape
  (setq dape-buffer-window-arrangement nil)

  (add-to-list 'dape-configs
               `(debugpy-poetry
                 modes (python-mode python-ts-mode)
                 command "poetry"
                 command-args ("run" "python" "-m" "debugpy.adapter")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-buffer-default
                 :stopOnEntry t))
  (add-to-list 'dape-configs
               `(debugpy-poetry-pytest
                 modes (python-mode python-ts-mode)
                 command "poetry"
                 command-args ("run" "python" "-m" "debugpy" "--listen" "0" "--wait-for-client" "-m" "pytest" "--pdb-trace" "-s")
                 :type "executable"
                 :request "launch"
                 :cwd dape-cwd-fn
                 :program dape-buffer-default
                 :stopOnEntry t)))
