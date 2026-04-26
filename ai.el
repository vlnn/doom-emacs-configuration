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
  (setq aider-program "aider")
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
  (setq gptel-model 'qwen3-coder:30b
        gptel-backend
        (gptel-make-ollama "Ollama"
          :stream t
          :models '(qwen3-coder:30b
                    qwen2.5-coder:32b-instruct
                    deepseek-r1:32b))))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)
