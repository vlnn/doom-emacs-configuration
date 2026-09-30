;;; my/ai/config.el -*- lexical-binding: t; -*-

;;; llama-server

(defconst +ai-llama-host "127.0.0.1:8080")
(defconst +ai-fast-model 'qwopus-coder)
(defconst +ai-think-model 'qwopus-reason)

(defconst +ai-llama-fallback-models
  '(qwopus-reason qwopus-coder qwopus-fast gpt-oss-20b))

(defun +ai--llama-url (path)
  (concat "http://" +ai-llama-host path))

(defun +ai--llama-model-ids (json)
  (mapcar (lambda (m) (intern (alist-get 'id m)))
          (alist-get 'data json)))

(defun +ai--llama-fetch-models ()
  (with-current-buffer (url-retrieve-synchronously (+ai--llama-url "/v1/models") t t 2)
    (goto-char url-http-end-of-headers)
    (let ((json-key-type 'symbol)
          (json-array-type 'list))
      (+ai--llama-model-ids (json-read)))))

(defun +ai--llama-models ()
  (or (ignore-errors (+ai--llama-fetch-models))
      +ai-llama-fallback-models))

(defun +ai--llama-backend ()
  (gptel-make-openai "llama-server"
    :host +ai-llama-host
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :key "llama-server"
    :stream t
    :models (+ai--llama-models)))

(defun +ai--install-llama-backend ()
  (setq gptel-backend (+ai--llama-backend)
        gptel-quick-backend gptel-backend
        gptel-magit-backend gptel-backend))

(defun +ai/refresh-llama-models ()
  "Re-read the served model list after editing llama-server/config.ini."
  (interactive)
  (+ai--install-llama-backend)
  (message "gptel models: %s" (gptel-backend-models gptel-backend)))

(set-popup-rule! "^\\*llama-server\\*$"
  :side 'right :size 0.4 :select t :quit nil :ttl nil)

;;; gptel

(defun +ai--text-at-point (&optional whole-defun)
  "Region when active, else the current line, or the defun when WHOLE-DEFUN."
  (string-trim-right
   (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
         (whole-defun (or (thing-at-point 'defun t) (thing-at-point 'line t)))
         (t (thing-at-point 'line t)))
   "\n"))

(after! gptel
  (setq gptel-model +ai-fast-model
        gptel-include-reasoning 'ignore
        gptel-rewrite-default-action 'dispatch)
  (+ai--install-llama-backend)
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
  (setq gptel-quick-model +ai-fast-model
        gptel-quick-word-count 24
        gptel-quick-timeout 60))

(defun +ai/quick (&optional whole-defun)
  "gptel-quick on the region, else the current line (defun with WHOLE-DEFUN)."
  (interactive "P")
  (gptel-quick (+ai--text-at-point whole-defun)))

(map! :leader :desc "Explain quickly" "o l e" #'+ai/quick)


;;; annotated explanation — SPC o l E

(load! "annotate")

;;; gptel-magit — commit messages and diff explanations

(defun +ai--gptel-magit-require-staged (fn &rest args)
  (if (string-empty-p (magit-git-output "diff" "--cached"))
      (user-error "Nothing staged; stage the changes you want described")
    (apply fn args)))

(after! gptel-magit
  (setq gptel-magit-model +ai-fast-model
        gptel-magit-body-length 72)
  (advice-add 'gptel-magit--generate :around #'+ai--gptel-magit-require-staged))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)

;;; agents

(use-package! aider
  :init
  (map! :leader :desc "aider" "1" #'aider-transient-menu)
  :config
  (require 'aider-doom)
  (setq aider-program "cecli"
        aider-args (list "--model" (format "openai/%s" +ai-fast-model)
                         "--openai-api-base" (+ai--llama-url "/v1")
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
