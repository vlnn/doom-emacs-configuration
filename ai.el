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
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  (setq aider-program "cecli"
        aider-args '("--model" "ollama_chat/qwopus-coder"))
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

(after! gptel
  (setq gptel-model 'qwopus-reason
        gptel-include-reasoning 'ignore
        gptel-backend
        (gptel-make-ollama "Ollama"
          :stream t
          :models '(qwopus-reason
                    qwopus-coder
                    qwen3-coder:30b
                    deepseek-r1:32b)))
  (setf (alist-get 'review gptel-directives)
        "You review code. Flag non-idiomatic constructs, missing or weak test cases, oversized functions, and asserts lacking explanation strings. Be terse."
        (alist-get 'plan gptel-directives)
        "You plan TDD work. Given a function or feature, list the failing tests to write first (in order) and the small named functions to implement. No code yet."
        (alist-get 'clojure gptel-directives)
        "You answer about idiomatic Clojure. Prefer threading macros, destructuring, and the seq library. Show minimal examples."))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)


(set-popup-rule! "^\\*Ollama\\*$"
  :side 'right :size 0.4 :select t :quit nil :ttl nil)

