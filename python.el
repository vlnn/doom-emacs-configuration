;;; python.el -*- lexical-binding: t; -*-

(use-package! lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright")
  :hook (python . (lambda ()
                    (require 'lsp-pyright)
                    (lsp-defered))))  ; or lsp-deferred



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
        lsp-headerline-breadcrumb-enable t))

(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t))

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
       :desc "run"       "r" #'pipenv-run))
