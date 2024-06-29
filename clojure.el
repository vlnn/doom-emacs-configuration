;;; clojure.el -*- lexical-binding: t; -*-

;; Clojure (and cider)

;; workaround to make tabs working for yasnippets
(defun clj-tab ()
  (interactive)
  (if (yas-active-snippets)
      (yas-next-field)
    (yas-expand)))

(map! :map prog-mode-map ; HACK not sure if it should be clojure-specific only, so it's open experiment for now
      :i "<tab>" #'clj-tab)

(after! cider
  (add-to-list 'exec-path "/home/va/.asdf/shims")
  (setq nrepl-use-ssh-fallback-for-remote-hosts t) ;; Cider should be able to connect to remote hosts using ssh
  (setq cider-eldoc-display-context-dependent-info t)
                                        ;(setq clojure-align-forms-automatically t)
  (map! :after clojure-mode :map clojure-mode-map :localleader ;; faster simpler workflow shortcuts
        ("f" #'consult-flycheck)
        ("m" #'cider-selector)
        ("P" nil)
        ("p" nil)
        ("\;" #'cider-pprint-eval-last-sexp-to-comment)
        ("N" #'cider-test-run-ns-tests)
        ("T" #'projectile-toggle-between-implementation-and-test))
  (setq cider-print-options '(("length" 20) ("right-margin" 70)))
  (subword-mode)
  (evil-make-intercept-map cider--debug-mode-map 'normal) ;; don't mess evil-mode with cider debug
  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps))

(after! cider
  :defer t
  (use-package! parinfer-rust-mode
    :defer t
    :init
    (setq parinfer-rust-mode 'paren
          parinfer-rust-library "~/.config/emacs/.local/etc/parinfer-rust/libparinfer_rust.dylib") ; due to MacOS on M1. Had to compile it and put in this folder.
    :config
    (map! :map parinfer-rust-mode-map
          :localleader
          "p" #'cider-pprint-eval-last-sexp
          "[" #'parinfer-rust-switch-mode
          "P" #'cider-pprint-eval-last-sexp-to-comment
          "{" #'parinfer-rust-toggle-disable)))

(add-to-list 'load-path (concat doom-user-dir "cider-storm"))
(require 'cider-storm)
