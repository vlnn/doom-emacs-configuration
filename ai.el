;;; ai.el -*- lexical-binding: t; -*-
;; AI assistants want fresh on-disk state; revert buffers automatically.
(global-auto-revert-mode 1)

;;; llama-server

(defconst my/llama-server-host "127.0.0.1:8080")
(defconst my/llm-fast-model 'qwopus-coder)
(defconst my/llm-think-model 'qwopus-reason)

(defconst my/llama-server-fallback-models
  '(qwopus-reason qwopus-coder qwopus-fast gpt-oss-20b))

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

(defun my/llama-server-install-backend ()
  (setq gptel-backend (my/llama-server-backend)
        gptel-quick-backend gptel-backend
        gptel-magit-backend gptel-backend))

(defun my/llama-server-refresh-models ()
  "Re-read the served model list after editing llama-server/config.ini."
  (interactive)
  (my/llama-server-install-backend)
  (message "gptel models: %s" (gptel-backend-models gptel-backend)))

(set-popup-rule! "^\\*llama-server\\*$"
  :side 'right :size 0.4 :select t :quit nil :ttl nil)

;;; gptel

(after! gptel
  (setq gptel-model my/llm-fast-model
        gptel-include-reasoning 'ignore
        gptel-rewrite-default-action 'dispatch)
  (my/llama-server-install-backend)
  (setf (alist-get 'review gptel-directives)
        "You review code. Flag non-idiomatic constructs, missing or weak test cases, oversized functions, and asserts lacking explanation strings. Be terse."
        (alist-get 'plan gptel-directives)
        "You plan TDD work. Given a function or feature, list the failing tests to write first (in order) and the small named functions to implement. No code yet."
        (alist-get 'test gptel-directives)
        "You write pytest tests. Parametrize aggressively, mock with pytest-mock (never unittest.mock), give every assert an explanation string of the form 'X should Y'. Small named helpers over fixtures with logic. Output only code."
        (alist-get 'clojure gptel-directives)
        "You answer about idiomatic Clojure. Prefer threading macros, destructuring, and the seq library. Show minimal examples."))

;;; gptel-quick — SPC o l e

(after! gptel
  (setq gptel-quick-model my/llm-fast-model
        gptel-quick-word-count 24
        gptel-quick-timeout 60))

;;; annotated explanation — SPC o l E

(defconst my/gptel-annotate-directive
  "Reproduce the given code verbatim. Before each meaningful line, insert one or more comment lines, in the language's own comment syntax, explaining what that line does, numbered 1., 2., ... in order. Keep each comment line under 60 characters; continue onto another comment line rather than exceeding it. For a line doing several things, name them left to right in evaluation order. Output only the annotated code: no prose, no code fences.")

(defun my/gptel-annotate-text ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'defun t)))

(defun my/gptel-annotate-buffer (mode)
  (with-current-buffer (get-buffer-create "*gptel-annotate*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p mode)
        (delay-mode-hooks (funcall mode)))
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))
    (current-buffer)))

(defun my/gptel-annotate-append (buffer text)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert text))))

(defun my/gptel-annotate-strip-fences (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (flush-lines "^\\s-*```"))))

(defun my/gptel-annotate-callback (response info)
  (let ((buffer (plist-get info :buffer)))
    (pcase response
      ((pred stringp) (my/gptel-annotate-append buffer response))
      ('t (my/gptel-annotate-strip-fences buffer))
      ('nil (message "Annotate failed: %s" (plist-get info :status))))))

(defun my/gptel-annotate ()
  "Show the region or defun at point with a numbered explanation per line."
  (interactive)
  (let* ((text (my/gptel-annotate-text))
         (buffer (my/gptel-annotate-buffer major-mode))
         (gptel-model my/llm-fast-model))
    (pop-to-buffer buffer)
    (gptel-request text
      :system my/gptel-annotate-directive
      :stream t
      :buffer buffer
      :callback #'my/gptel-annotate-callback)))

(set-popup-rule! "^\\*gptel-annotate\\*$"
  :side 'right :size 0.5 :select t :quit t :ttl nil)

(map! :leader :desc "Explain annotated" "o l E" #'my/gptel-annotate)

;;; gptel-magit — commit messages and diff explanations

(after! gptel-magit
  (setq gptel-magit-model my/llm-fast-model
        gptel-magit-body-length 72))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)

;;; agents

(use-package! aider
  :init
  (key-chord-define-global "12" 'aider-transient-menu)
  (map! :leader :desc "aider" "1" #'aider-transient-menu)
  :config
  (require 'aider-doom)
  (setq aider-program "cecli"
        aider-args (list "--model" (format "openai/%s" my/llm-fast-model)
                         "--openai-api-base" (my/llama-server-url "/v1")
                         "--openai-api-key" "llama-server"
                         "--no-show-model-warnings"))
  (set-popup-rule! "^\\*aider"   :quit nil)
  (set-popup-rule! "^\\*Python\\*" :quit nil))

(use-package! ai-code
  :config
  (ai-code-set-backend 'aider)
  (setq ai-code-menu-layout 'two-columns
        ai-code-auto-test-type 'ask-me)
  (map! "C-c a" #'ai-code-menu)
  (ai-code-prompt-filepath-completion-mode 1)
  (after! evil  (ai-code-backends-infra-evil-setup))
  (after! magit (ai-code-magit-setup-transients)))

(use-package! opencode
  :init
  (map! :leader :desc "opencode" "2" #'opencode)
  :config
  (setq opencode-default-model "anthropic/claude-sonnet-4-6")
  (set-popup-rule! "^\\*opencode" :side 'right :size 0.4 :select t :quit nil :ttl nil))

