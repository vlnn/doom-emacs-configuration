;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Fantasque Sans Mono")
(setq doom-font (font-spec :family font-family :size 15)
      doom-unicode-font (font-spec :family font-family :size 15)
      doom-big-font (font-spec :family font-family :size 20))

;; Using Macbook is hard. But we'll manage
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control))

;; The best clean light theme I've found yet
(setq doom-theme 'doom-homage-white)

;; I like absolute line numbers. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; default undo settings are too aggregative to my likings -- undoing these!
;; NB: This is important! Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; Movements schema change, unusual for many
;; Move cursor with 'jkl;', not default evil 'hjkl'
;; Change evil-repeat keys...
(setq evil-snipe-override-evil-repeat-keys nil)
;; ...and then use jkl; instead of hjkl. It's stupid how hard it is to re-map ;!
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map "\;" 'evil-forward-char)
  (define-key evil-motion-state-map "k" 'evil-next-line)
  (define-key evil-motion-state-map "l" 'evil-previous-line))

;; use similar layout for window controls
(map! :map evil-window-map
      ;; Navigation using jkl; not hjkl
      "j"         #'evil-window-left
      "k"         #'evil-window-down
      "l"         #'evil-window-up
      ";"         #'evil-window-right
      ;; Swapping windows using jkl; not hjkl
      "C-h"       #'+evil/window-move-left
      "C-k"       #'+evil/window-move-down
      "C-\;"      #'+evil/window-move-right)

;; Prog languages modes preferences
;; Common mapping
(map! :leader
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; configure (custom-built) parinfer
(setq parinfer-rust-library "/Users/va/.config/emacs/.local/etc/parinfer-rust/libparinfer_rust.dylib")

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|())
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
;; every windows should be avy'ed
(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t)
  (key-chord-define-global "jk" 'avy-goto-char-timer))

;; Use simplier window setup
(current-window-only-mode)

;; Clojure (and cider)
(after! cider
  (add-to-list 'exec-path "/home/va/.asdf/shims")
  (setq nrepl-use-ssh-fallback-for-remote-hosts t) ;; Cider should be able to connect to remote hosts using ssh
  (setq cider-eldoc-display-context-dependent-info t)
  (setq clojure-align-forms-automatically t)
  (map! :after clojure-mode :map clojure-mode-map :localleader ;; faster simpler workflow shortcuts
        ("f" #'consult-flycheck)
        ("m" #'cider-selector)
        ("\;" #'cider-pprint-eval-last-sexp-to-comment)
        ("T" #'projectile-toggle-between-implementation-and-test))
  (evil-make-intercept-map cider--debug-mode-map 'normal) ;; don't mess evil-mode with cider debug
  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps))

;; To kill trailing whitespaces... but being sane killer not removing whitespace I just typed in
(use-package! ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point t))

;; TABs should never be inserted at all
;; (I might need to change it for python or YAML -- but hopefully I don't need them)
(setq-default indent-tabs-mode nil)

;; TAB should work both for line indent and autocompletion
(setq-default c-basic-offset tab-width)
(setq tab-always-indent 'complete)

;; try to enable project-search over TRAMP
(after! projectile
  (setq projectile-mode-line "Projectile")
  (setq projectile-enable-caching t)
  (setq projectile-project-search-path '("~/tmp/"  ("~/src" . 1))))

;; Better git blame
(use-package! blamer
  :bind (("M-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 110 ;; I don't know how to set "smaller than normal" height here, so just hardcoded for now
                   :italic t)))
  :config
  (setq blamer-type 'both))

;; to see saved but not committed things in all the files
(global-blamer-mode 1)
