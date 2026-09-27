;;; ai.el -*- lexical-binding: t; -*-
;; AI assistants want fresh on-disk state; revert buffers automatically.
(global-auto-revert-mode 1)
(setq auto-revert-interval 1)

(use-package! aider
  :init
  (require 'aider-helm)
  (key-chord-define-global "12" 'aider-transient-menu)
  (map! :leader :desc "aider" "1" #'aider-transient-menu)
  :config
  (require 'aider-doom)
  (setenv "OPENAI_API_BASE" "http://127.0.0.1:8080/v1")
  (setenv "OPENAI_API_KEY" "llama-server")
  (setq aider-program "cecli"
        aider-args '("--model" "openai/qwopus-coder"))
  (set-popup-rule! "^\\*aider"   :quit nil)
  (set-popup-rule! "^\\*Python\\*" :quit nil))

(use-package! ai-code
  :config
  (ai-code-set-backend 'aider)
  (setq ai-code-menu-layout 'two-columns
        ai-code-auto-test-type 'ask-me)
  (global-set-key (kbd "C-c a") #'ai-code-menu)
  (ai-code-prompt-filepath-completion-mode 1)
  (with-eval-after-load 'evil  (ai-code-backends-infra-evil-setup))
  (with-eval-after-load 'magit (ai-code-magit-setup-transients)))

(use-package! mindstream
  :config (mindstream-mode))

(defconst my/llama-server-host "127.0.0.1:8080")

(defconst my/llama-server-fallback-models
  '(qwopus-reason qwopus-coder qwen3-coder:30b deepseek-r1:32b))

(defun my/llama-server-url (path)
  (concat "http://" my/llama-server-host path))

(defun my/llama-server-model-ids (json)
  (mapcar (lambda (m) (intern (alist-get 'id m)))
          (alist-get 'data json)))

(defun my/llama-server-fetch-models ()
  (with-current-buffer (url-retrieve-synchronously (my/llama-server-url "/v1/models") t t 2)
    (goto-char url-http-end-of-headers)
    (let ((json-key-type 'symbol)
          (json-array-type 'list))
      (my/llama-server-model-ids (json-read)))))

(defun my/llama-server-models ()
  (or (ignore-errors (my/llama-server-fetch-models))
      my/llama-server-fallback-models))

(defun my/llama-server-backend ()
  (gptel-make-openai "llama-server"
    :host my/llama-server-host
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :key "llama-server"
    :stream t
    :models (my/llama-server-models)))

(defun my/llama-server-refresh-models ()
  "Re-read the served model list after editing llama-server/config.ini."
  (interactive)
  (setq gptel-backend (my/llama-server-backend))
  (message "gptel models: %s" (gptel-backend-models gptel-backend)))

(after! gptel
  (setq gptel-model 'qwopus-reason
        gptel-include-reasoning 'ignore
        gptel-backend (my/llama-server-backend))
  (setf (alist-get 'review gptel-directives)
        "You review code. Flag non-idiomatic constructs, missing or weak test cases, oversized functions, and asserts lacking explanation strings. Be terse."
        (alist-get 'plan gptel-directives)
        "You plan TDD work. Given a function or feature, list the failing tests to write first (in order) and the small named functions to implement. No code yet."
        (alist-get 'clojure gptel-directives)
        "You answer about idiomatic Clojure. Prefer threading macros, destructuring, and the seq library. Show minimal examples."))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)


(set-popup-rule! "^\\*llama-server\\*$"
  :side 'right :size 0.4 :select t :quit nil :ttl nil)


(use-package! opencode
  :init
  (map! :leader :desc "opencode" "2" #'opencode)
  :config
  (setq opencode-default-model "anthropic/claude-sonnet-4-6")
  (set-popup-rule! "^\\*opencode" :side 'right :size 0.4 :select t :quit nil :ttl nil))
