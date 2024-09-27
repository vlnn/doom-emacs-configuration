;;; gptel.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config (setq
           gptel-model   "llama-3.1-sonar-small-128k-online"
           gptel-backend (gptel-make-openai "Perplexity"
                           :host "api.perplexity.ai"
                           :key perplexity-ai-api
                           :endpoint "/chat/completions"
                           :stream t
                           :models '("llama-3.1-sonar-small-128k-online"
                                     "llama-3.1-sonar-large-128k-chat"
                                     "llama-3.1-sonar-large-128k-online"))))
