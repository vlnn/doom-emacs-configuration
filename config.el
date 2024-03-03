;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Iosevka Term")
(setq doom-font (font-spec :family font-family :size 13)
      doom-symbol-font (font-spec :family font-family :size 13)
      doom-big-font (font-spec :family font-family :size 19))

;; Using Macbook is hard. But we'll manage
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control
        dired-use-ls-dired nil))

;; Start as big as possible
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; The best clean light theme I've found yet
(use-package! stimmung-themes
  :demand t
  :config
  (stimmung-themes-load-light))

;; I like absolute line numbers. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; I don't like how word-wrap is working
(setq word-wrap nil)

;; default undo settings are too aggregative to my likings -- undoing these!
;; NB: This is important! Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; how many times did you SPC m e e
;; just to see result of some internal sexp evaluation?
;; Too many, not anymore!
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)
(setq evil-highlight-closing-paren-at-point-states nil)

;; Colors and blinks setup
(blink-cursor-mode 1)
(set-cursor-color "dark green")
(setq-default line-spacing 1)
(use-package! hl-line
  :custom-face
  (hl-line ((t (:background "#d5f7d5")))))
(mouse-avoidance-mode 'animate)
(setq mouse-avoidance-threshold 0.5)

(setq completion-ignored-extensions
      '(".a"
        ".aux"
        ".bbl"
        ".bin"
        ".blg"
        ".class"
        ".cp"
        ".cps"
        ".elc"
        ".fmt"
        ".fn"
        ".fns"
        ".git/"
        ".glo"
        ".glob"
        ".gmo"
        ".hg/"
        ".idx"
        ".ky"
        ".kys"
        ".la"
        ".lib"
        ".ln"
        ".lo"
        ".lof"
        ".lot"
        ".mem"
        ".mo"
        ".o"
        ".pg"
        ".pgs"
        ".pyc"
        ".pyo"
        ".so"
        ".tfm"
        ".toc"
        ".tp"
        ".tps"
        ".v.d"
        ".vio"
        ".vo" ".vok" ".vos"
        ".vr"
        ".vrs"
        "~"))

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

(after! winum
  (setq winum-scope 'visible
        winum-auto-setup-mode-line t))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

;; General preferences
;; Common mapping
(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace" "%" #'query-replace
      :desc "Undo abbrev"   "U" #'unexpand-abbrev
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|())
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
;; every windows should be avy'ed
(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t)
  (key-chord-define-global "jk" 'avy-goto-char-timer))

(load! "clojure.el")

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

(load! "magit.el")
(load! "org-mode.el")
(load! "json.el")

(use-package! abbrev
  :config
  (setq abbrev-file-name (concat doom-user-dir "abbrev_defs"))
  (setq save-abbrevs 'silently))
(setq-default abbrev-mode 1)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        evil-forward-char
        evil-backward-char
        evil-previous-line
        evil-delete-backward-char-and-join
        next-line))

(load! "codeium.el")

(use-package! why-this
  :hook (prog-mode . why-this-mode)
  :config
  (setq why-this-annotate-author-length 1
        why-this-annotate-width 5)
  (set-face-background 'why-this-annotate-heat-map-cold "#0de3f4")
  (set-face-background 'why-this-face "#f3fff4")
  (set-face-foreground 'why-this-face "#7d8d9d"))

(use-package! beacon
 :config
 (setq beacon-size 80
       beacon-blink-delay 0.1
       beacon-blink-when-focused 't
       beacon-blink-when-point-moves-vertically 3))

(use-package! lsp
  :config
  ; this should make stuff faster... so I've been told.
  (setq font-lock-maximum-decoration 1))

(use-package! org-shortcut)
(load! "secrets.el")

