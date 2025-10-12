;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it
;;
;; Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
;; (setq font-family "Iosevka Term")
(setq font-family "M+1Code Nerd Font Mono")
(setq doom-font (font-spec :family font-family :size 13 :weight 'regular)
      doom-symbol-font (font-spec :family font-family :size 13 :weight 'thin)
      doom-big-font (font-spec :family font-family :size 19 :weight 'regular))
(after! doom-themes
  (custom-set-faces!
    '(font-lock-keyword-face :weight regular))
  (doom/reload-font))

;; Using Macbook is hard. But we'll manage
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control
        dired-use-ls-dired nil))

;; Start as big as possible
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq gc-cons-threshold most-positive-fixnum)


;; I don't like to comment out block of lisp with ;
(defmacro comment (&rest _body)
  "Comment out one or more s-expressions."
  nil)

(setq lsp-copilot-enabled nil)

(setq doom-theme 'doom-flatwhite)
                                        ; (setq doom-theme 'doom-earl-grey)

(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

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
(visual-fill-column-mode -1)
(setq visual-fill-column-split-window-sensibly t)
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

(map! :leader
      :desc "ace-window"
      "W" #'ace-window)


;; generalize movements in result of SPC SPC, g D etc.
(map! :map (minibuffer-mode-map
            ivy-minibuffer-map
            vertico-map)
      :g "C-k" #'next-line
      :g "C-l" #'previous-line)

(after! winum
  (setq winum-scope 'visible
        winum-auto-setup-mode-line t))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable))
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; General preferences
;; Common mapping
(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace" "%" #'query-replace
      :desc "Undo abbrev"   "U" #'unexpand-abbrev
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|())
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t
        avy-single-candidate-jump nil)

  ;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
  ;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than g-s-/
  ;; every windows should be avy'ed
  (key-chord-define-global "jk" 'avy-goto-char-timer)           ; default and most useful movement
  (key-chord-define-global "bn" 'frog-jump-buffer)              ; get another way of quick switching buffers
  (key-chord-define-global "ji" 'evil-avy-goto-line)            ; not sure if it's better than the <line number> G or <line number> gg
  (key-chord-define-global "78" 'sp-beginning-of-previous-sexp) ; get to the beginning of the prev sexp
  (key-chord-define-global "89" 'sp-beginning-of-sexp)          ; get to the beginning of the sexp
  (key-chord-define-global "90" 'sp-end-of-sexp)                ; get to the end of the sexp
  (key-chord-define-global "0-" 'sp-end-of-next-sexp)           ; get to the end of next sexp
  (key-chord-define-global "sd" 'basic-save-buffer)             ; too much of shift-;-w-q-<ENT> in my life
  (key-chord-define-global "mn" 'dap-hydra)                     ; hydra for debugging
  (key-chord-define-global "j\;" 'execute-extended-command))     ; same as M-x, to run emacsy command, but specific for the buffer

(load! "avy-functions.el")
(after! avy
  (setq avy-timeout-seconds 0.7)
  :custom
  (set-face-attribute 'avy-lead-face-0 nil :foreground "black" :background "yellow green")
  (set-face-attribute 'avy-lead-face nil :foreground "black" :background "lime green")
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; don't use home row for mapping avy functions in avy-dispactch-alist!
  :config
  (avy-setup-default)

  (setf (alist-get ?x avy-dispatch-alist) 'avy-action-kill-whole-sexp
        (alist-get ?X avy-dispatch-alist) 'avy-action-kill-whole-defun
        (alist-get ?e avy-dispatch-alist) 'avy-action-clojure-eval-whole-sexp
        (alist-get ?E avy-dispatch-alist) 'avy-action-clojure-eval-whole-defn
        (alist-get ?q avy-dispatch-alist) 'avy-action-rename-sexp          ; jump to and replace sexp (e.g. change function no touching args)
        (alist-get ?Q avy-dispatch-alist) 'avy-action-rename-whole-sexp    ; jump to and replace sexp (e.g. change function no touching args)
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-clone-whole-sexp
        (alist-get ?W avy-dispatch-alist) 'avy-action-clone-whole-defun
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport-whole-sexp
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-defun
        (alist-get ?z avy-dispatch-alist) 'avy-action-zap-to-char
        (alist-get ?m avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?c avy-dispatch-alist) 'avy-action-comment-whole-sexp   ; comment out whole sexp avy'ed
        (alist-get ?C avy-dispatch-alist) 'avy-action-comment-whole-defn   ; comment out whole defn avy'ed
        (alist-get ?i avy-dispatch-alist) 'avy-action-lookup-documentation
        (alist-get ?r avy-dispatch-alist) 'avy-action-lookup-references
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(load! "clojure.el")

;; TABs should never be inserted at all
;; (I might need to change it for python or YAML -- but hopefully I don't need them)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset tab-width)
(setq tab-always-indent nil)

(load! "magit.el")

(load! "org-mode.el")

(load! "json.el")

(use-package! abbrev
  :config
  (setq abbrev-file-name (concat doom-user-dir "abbrev_defs"))
  (setq save-abbrevs 'silently))
(setq-default abbrev-mode 1)

(use-package! demo-it
  :config
  (define-key demo-it-mode-map "<f12>" 'demo-it-step)
  (map! :map demo-it-mode-map
        "<f12>" #'demo-it-step))

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        evil-forward-char
        evil-backward-char
        evil-previous-line
        evil-delete-backward-char-and-join
        next-line))


(use-package! beacon
  :config
  (setq beacon-size 80
        beacon-blink-delay 0.1
        beacon-blink-when-focused 't
        beacon-blink-when-point-moves-vertically 3))

(load! "secrets.el")
(load! "timers.el")

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))

  (select-frame-by-name "remember")
  (org-capture))


(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(load! "blog.el")

(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"    #'drag-stuff-up
        "<M-down>"  #'drag-stuff-down))


(use-package! mini-ontop
  :ensure t
  :config (mini-ontop-mode 1)
  (setq mini-ontop-lines 40)) ; this is magic number working on my setup. Improvement a bit too miniscule to fix it properly.

(load! "dape.el")

(use-package! aider
  :init
  (require 'aider-helm)
  (key-chord-define-global "12" 'aider-transient-menu)
  (map! :leader :desc "aider" "1" #'aider-transient-menu)

  :config
  (require 'aider-doom)
  (setq aider-program "aider")
  (set-popup-rule! "^\\*aider" :quit nil)
  (set-popup-rule! "^\\*Python\\*" :quit nil))

(use-package! mindstream
  :config
  (mindstream-mode))

(use-package! casual)

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/org/denote/"))
  (setq denote-known-keywords '("emacs" "journal" "project" "idea"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; org by default
  (setq denote-prompts '(title keywords)))

(use-package! denote-projectile-notes
  :after (denote projectile)
  :config
  (map! :leader
        :prefix "p"
        :desc "Project notes" "n" #'denote-project-notes
        :desc "New project note" "N" #'denote-create-project-note))

(after! projectile
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf))

(use-package! ob-duckdb)

(after! plantuml-mode
  ;; Use executable mode instead of jar
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-executable-path "plantuml") ; or full path if not in PATH

  ;; Enable org-babel support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))

  ;; Set executable path for org-babel too
  (setq org-plantuml-executable-path "plantuml")
  (setq org-plantuml-exec-mode 'executable))


(use-package! elfeed-score
  :ensure t
  :config
  (progn
    (elfeed-score-enable)
    (define-key elfeed-search-mode-map "=" elfeed-score-map)))


(turn-on-solaire-mode)

