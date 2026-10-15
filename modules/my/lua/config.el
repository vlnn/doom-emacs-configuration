;;; my/lua/config.el -*- lexical-binding: t; -*-

;; lua-mode wired the way the (clojure +lsp) module wires cider: results
;; inline, printed output in the REPL, cider-shaped localleader, busted tests.

(load! "repl")
(load! "busted")
(load! "love")

(after! lua-mode
  (set-repl-handler! 'lua-mode #'+lua/open-project-repl
    :persist t
    :send-region #'+lua/eval-region
    :send-buffer #'+lua/load-buffer)
  (set-eval-handler! 'lua-mode #'+lua/eval-region)

  (set-popup-rule! "^\\*lua\\*$" :quit nil :ttl nil)

  (setq-default lua-default-command-switches (+lua--repl-switches +lua-interrupt-file))
  (advice-add 'lua-start-process :after #'+lua--enable-repl-mode)
  (when (modulep! :editor evil)
    (add-hook '+lua-repl-mode-hook #'evil-normalize-keymaps))

  (map! :map lua-mode-map
        "C-x C-e" #'+lua/eval-line-or-region
        "C-M-x"   #'+lua/eval-defun
        "C-c C-c" #'+lua/eval-defun
        "C-c C-e" #'+lua/eval-line-or-region
        "C-c C-k" #'+lua/load-buffer
        "C-c C-b" #'+lua/interrupt
        "C-c C-z" #'+lua/switch-to-repl)

  (map! :localleader
        :map lua-mode-map
        "'" #'+lua/switch-to-repl
        (:prefix ("e" . "eval")
         "b" #'+lua/load-buffer
         "d" #'+lua/eval-defun
         "D" #'+lua/insert-defun-in-repl
         "e" #'+lua/eval-line-or-region
         "E" #'+lua/insert-line-or-region-in-repl
         "r" #'+lua/eval-region
         "R" #'+lua/insert-line-or-region-in-repl)
        (:prefix ("g" . "goto")
         "g" #'+lookup/definition)
        (:prefix ("h" . "help")
         "d" #'lua-search-documentation)
        (:prefix ("p" . "print")
         "p" #'+lua/eval-line-or-region-to-popup
         "P" #'+lua/eval-line-or-region-to-comment)
        (:prefix ("r" . "repl")
         "b" #'+lua/switch-to-repl
         "c" #'+lua/clear-repl
         "i" #'+lua/interrupt
         "l" #'+lua/load-buffer
         "L" #'+lua/load-buffer-and-switch-to-repl
         "q" #'+lua/quit-repl
         "R" #'+lua/restart-repl)
        (:prefix ("t" . "test")
         "a" #'recompile
         "n" #'+lua/test-file
         "p" #'+lua/test-project
         "t" #'+lua/test-at-point))

  (map! :map +lua-repl-mode-map
        "C-c C-c" #'+lua/interrupt
        "C-c C-z" #'+lua/switch-to-last-source)

  (map! :localleader
        :map +lua-repl-mode-map
        "b" #'+lua/switch-to-last-source
        "c" #'+lua/clear-repl
        "i" #'+lua/interrupt
        "q" #'+lua/quit-repl
        "R" #'+lua/restart-repl))
