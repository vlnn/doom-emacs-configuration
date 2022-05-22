;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; {{{ Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Fantasque Sans Mono")
(setq doom-font (font-spec :family font-family :size 17)
      doom-unicode-font (font-spec :family font-family :size 17)
      doom-variable-pitch-font (font-spec :family "DejaVu Math TeX Gyre" :size 17)
      doom-big-font (font-spec :family font-family :size 20))

;; Ugly HACK to make Ukrainian and English fonts really the same
(add-hook 'text-mode-hook (lambda () (doom/reload-font)))

;; best clean light theme I've found yet
(setq doom-theme 'doom-homage-white)

;; I like absolute line numbers. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; I need to be able to toggle between english and ukrainian inputs
(set-input-method "ukrainian-computer")

;; HACK tool below should be installed (manually -- or rather somewhere else, not in emacs)
(setq langtool-language-tool-jar "~/bin/LanguageTool-5.6-stable/languagetool-commandline.jar")
(setq ispell-program-name (executable-find "hunspell"))

;; Flyspell should not put all red into "wrong language"
(use-package guess-language         ; Automatically detect language for Flyspell
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (uk . ("uk_UA" "Ukrainian")))
        guess-language-languages '(en uk)
        guess-language-min-paragraph-length 45)
  :diminish guess-language-mode)

;; Show me where the cursor is... Star Trek style!
(use-package! beacon)
(after! beacon
  (beacon-mode 1)
  (setq beacon-blink-when-focused t                    ;; beam me if I move to another window
        beacon-blink-when-point-moves-vertically 3     ;; beam me if I jump more than 2 lines away
        beacon-blink-when-point-moves-horizontally nil ;; but not if I move cursor around))
        ))

;; default undo settings are too aggregative to my likings -- undoing these!
;; NB: This is important! Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; To see trailing whitespaces
(setq-default show-trailing-whitespace t)

;; To kill trailing whitespaces... but being sane killer not removing whitespace I just typed in
(use-package! ws-butler
  :hook (doom-first-buffer . ws-butler-global-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point t))

;; Save buffers automatically
;; If you focus out of emacs it will save your work. How cool is that!
(use-package! super-save)
(after! super-save
  (super-save-mode +1)
  (add-to-list 'super-save-triggers 'ace-window)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))
;; Mappings for things I've added instead of finding how to do it correctly:
;; First change evil-repeat keys...
(setq evil-snipe-override-evil-repeat-keys nil)
;; ...and then use jkl; instead of hjkl. It's stupid how hard it is to re-map ;!
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map ";" 'evil-forward-char)
  (define-key evil-motion-state-map "k" 'evil-next-line)
  (define-key evil-motion-state-map "l" 'evil-previous-line))

;; use similar layout for window controls
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation using jkl; not hjkl
      "j"     #'evil-window-left
      "k"     #'evil-window-down
      "l"       #'evil-window-up
      ";"    #'evil-window-right
      ;; Swapping windows using jkl; not hjkl
      "C-h"       #'+evil/window-move-left
      "C-k"       #'+evil/window-move-down
      "C-;"      #'+evil/window-move-right)

;; gather some stats on keypresses
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        forward-char
        backward-char
        previous-line
        next-line))

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
  (setq projectile-project-search-path '("~/tmp/"  ("~/src" . 1)))
  (advice-remove #'projectile-locate-dominating-file #'doom*projectile-locate-dominating-file))

;; map hippie-expand to the shorcut
(map! :i "M-/" #'hippie-expand)

;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
(use-package avy
  :bind* (("C-j" . avy-goto-char-timer)))

;; every windows should be avy'ed
(setq avy-all-windows t)

;; Keycast should show every key/command at the mode line
(after! keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update))))
(add-to-list 'global-mode-string '("" keycast-mode-line))
(keycast-mode) ;; or run keycast-mode by demand
;;}}}

;; {{{ Org-mode stuff
(after! org
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/org/refile.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("r" "respond" entry (file "~/org/refile.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/org/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/org/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/org/refile.org")
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("h" "Habit" entry (file "~/org/refile.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "blue" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("HOLD" :foreground "magenta" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold)
                ("MEETING" :foreground "forest green" :weight bold)
                ("PHONE" :foreground "forest green" :weight bold))))

  (setq +org-capture-frame-parameters '((name . "doom-capture")
                                        (width . 170)
                                        (height . 110)
                                        (transient . t)
                                        (menu-bar-lines . 1)))

  (map! :leader
        :desc "Org capture" "X" #'+org-capture/open-frame))

;; I would like org clock to store info between emacs restarts
(require 'org-clock)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; try verb package to serve REST/HTTP requests
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
;; }}}

;;{{{ Prog languages modes preferences

;; Clojure
(after! cider
  (setq nrepl-use-ssh-fallback-for-remote-hosts t) ;; Cider should be able to connect to remote hosts using ssh
  (setq cider-default-repl-command "lein")
  (map! :after clojure-mode :map clojure-mode-map :localleader ;; faster simpler workflow shortcuts
        ("f" #'consult-flycheck)
        ("m" #'cider-selector)
        ("T" #'projectile-toggle-between-implementation-and-test))
  (add-hook 'cider-repl-mode-hook #'evil-smartparens-mode)
  (add-hook 'cider-popup-buffer-mode-hook #'evil-smartparens-mode)
  (evil-make-intercept-map cider--debug-mode-map 'normal)) ;; don't mess evil-mode with cider debug

;; Let's try computer to be agressive while indenting clojure
(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))

;; Smart parens are great to move around in lisp modes
(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-M-}" #'sp-forward-slurp-sexp
        "C-M-{" #'sp-forward-barf-sexp
        "C-M-[" #'sp-backward-slurp-sexp
        "C-M-]" #'sp-backward-barf-sexp)

  ;; e.g. > slurps forwards, < barfs backwards,  M-[({ or M-})] surrounds form in [{( or )}] etc
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'evil-smartparens-mode)
  (add-hook 'clojure-mode-hook #'evil-smartparens-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  )
;;}}}


;;; {{{ Block of custom functions and their bindings
(load! "abbrev.el")
(load! "custom.el")
(global-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp)
;;}}}

;;{{{ Zetteldeft: return of emacsian Luhmann
(use-package deft
  :custom
  (deft-extensions '("org" "md" txt))
  (deft-text-mode 'org-mode)
  (deft-directory "~/Private/Zettels")
  (deft-default-extension "org")
  (deft-use-filename-as-t title))

(use-package! zetteldeft
  :init
  (map! :leader
        :prefix "d"
        :desc "deft" "d" #'deft
        :desc "deft-refresh" "R" #'deft-refresh
        :desc "zetteldeft-deft-new-search" "D" #'zetteldeft-deft-new-search
        :desc "zetteldeft-search-at-point" "s" #'zetteldeft-search-at-point
        :desc "zetteldeft-search-current-id" "c" #'zetteldeft-search-current-id
        :desc "zetteldeft-follow-link" "g" #'zetteldeft-follow-link
        :desc "zetteldeft-avy-file-search-ace-window" "F" #'zetteldeft-avy-file-search-ace-window
        :desc "zetteldeft-avy-link-search" "l" #'zetteldeft-avy-link-search
        :desc "zetteldeft-avy-tag-search" "t" #'zetteldeft-avy-tag-search
        :desc "zetteldeft-tag-buffer" "T" #'zetteldeft-tag-buffer
        :desc "zetteldeft-find-file-id-insert" "i" #'zetteldeft-find-file-id-insert
        :desc "zetteldeft-find-file-full-title-insert" "I" #'zetteldeft-find-file-full-title-insert
        :desc "zetteldeft-find-file" "f" #'zetteldeft-find-file
        :desc "zetteldeft-new-file" "n" #'zetteldeft-new-file
        :desc "zetteldeft-new-file-and-backlink" "N" #'zetteldeft-new-file-and-backlink
        :desc "edun/deft-open-preview" "p" #'edun/deft-open-preview
        :desc "edun/deft-open-other" "v" #'edun/deft-open-other
        :desc "zetteldeft-file-rename" "r" #'zetteldeft-file-rename
        :desc "zetteldeft-count-words" "x" #'zetteldeft-count-words)
  :config
  (defun edun/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window))
  (defun edun/deft-open-other ()
    (interactive)
    (deft-open-file-other-window t))

  (font-lock-add-keywords
   'org-mode
   `((,zetteldeft-id-regex  . font-lock-warning-face)
     (,zetteldeft-tag-regex . font-lock-warning-face))))
;;}}}


(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
