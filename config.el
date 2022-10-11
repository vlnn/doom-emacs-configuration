;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; {{{ Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Fantasque Sans Mono")
(setq doom-font (font-spec :family font-family :size 17)
      doom-unicode-font (font-spec :family font-family :size 17)
      doom-big-font (font-spec :family font-family :size 20))

;; My name is?
(setq user-full-name "Volodymyr Anokhin"
      user-mail-address "vlnn-github@proton.me")

;; Ugly HACK to make Ukrainian and English fonts really the same
(add-hook 'text-mode-hook (lambda () (doom/reload-font)))

;; Using Macbook is hard.
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control))

;; best clean light theme I've found yet
(setq doom-theme 'doom-homage-white)

;; I like absolute line numbers. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; I need to be able to toggle between english and ukrainian inputs
(set-input-method "ukrainian-computer")

;; I want to return to the last location I have been editing in file when reopen it
(save-place-mode 1)

;; Watch for the files changes if they are open in buffers and reload them if changed
(global-auto-revert-mode 1)
;; Also for dired (e.g. we added or removed a file from open directory
(setq global-auto-revert-non-file-buffers t)

;; HACK tool below should be installed (manually -- or rather somewhere else, not in emacs)
(setq langtool-language-tool-jar "~/bin/LanguageTool/languagetool-commandline.jar")
(setq ispell-program-name (executable-find "hunspell"))

;; Flyspell should not put all red into "wrong language"
(use-package! guess-language         ; Automatically detect language for Flyspell
  :defer t
  :init (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_GB" "English"))
                                   (uk . ("uk" "Ukrainian")))
        guess-language-languages '(en uk)
        guess-language-min-paragraph-length 45)
  :diminish guess-language-mode)

;; Show me where the cursor is... Star Trek style!
;; (beacon-mode 1)
;; (setq beacon-blink-when-focused t                    ;; beam me if I move to another window
;;       beacon-blink-when-point-moves-vertically 3     ;; beam me if I jump more than 2 lines away
;;       beacon-blink-when-point-moves-horizontally nil ;; but not if I move cursor around
;;       )

;; default undo settings are too aggregative to my likings -- undoing these!
;; NB: This is important! Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; To see trailing whitespaces
(setq-default show-trailing-whitespace t)

;; To kill trailing whitespaces... but being sane killer not removing whitespace I just typed in
(use-package! ws-butler
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
  ;; (advice-remove #'projectile-locate-dominating-file #'doom*projectile-locate-dominating-file)
  )

;; map hippie-expand to the shorcut
(map! :i "M-/" #'hippie-expand)

;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
;; every windows should be avy'ed
(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t)
  (key-chord-define-global "jk" 'avy-goto-char-timer)
  )
;;}}}

(use-package! anki-editor
  :commands (anki-editor-mode)
  :init
  (map! :leader
        :desc "Anki Push tree"
        "m a p" #'anki-editor-push-tree)
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t
        anki-editor-break-consecutive-braces-in-latex t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

;; {{{ Org-mode stuff
(after! org
  (add-to-list 'org-modules 'org-habit)
  (setq org-agenda-files (list "~/org" "~/Sync")
        org-tags-column -80
        cfw:org-overwrite-default-keybinding t
        )
  (setq org-log-into-drawer "LOGBOOK") ;; This is due to Orgzly doing habits logging into LOGBOOK drawer
  (setq org-agenda-span 5
        org-agenda-start-day "-2d"
        org-agenda-start-with-clockreport-mode nil ; this by some reason doesn't work in doom... yet. See https://github.com/doomemacs/doomemacs/issues/
        org-agenda-start-with-log-mode t
        org-agenda-start-with-follow-mode t
        org-agenda-include-diary t
        org-habit-show-all-today t
        org-habit-show-done-always-green t
        org-habit-preceding-days 7
        org-habit-following-days 2
        org-use-property-inheritance t
        )
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/Sync/inbox.org")
                 "* TODO %?\n%U\n%aSCHEDULED: %t\n" :clock-in t :clock-resume t :prepend t)
                ("r" "respond" entry (file "~/org/refile.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                ("n" "note" entry (file "~/org/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (file+datetree "~/org/diary.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)
                ("w" "org-protocol" entry (file "~/Sync/inbox.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)
                ("m" "Meeting" entry (file "~/org/meetings.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                ("p" "Phone call" entry (file "~/org/meetings.org")
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                ("a" "Anki card" entry
                 (file+headline "~/org/anki/anki-mega.org" "Anki Basic")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n")
                ("A" "Anki cloze" entry
                 (file+headline "~/org/anki/anki-mega.org" "Anki Cloze")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n")
                ("h" "Habit" entry (file "~/Sync/les oulitas.org")
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
                                        (transient . t)
                                        (menu-bar-lines . 1)))

  (map! :leader
        :desc "Org capture" "X" #'+org-capture/open-frame)

  ;; I would like org clock to store info between emacs restarts
  (require 'org-clock)
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate)

  ;; (use-package! org-super-agenda
  ;;   :after org-agenda
  ;;   :config
  ;;   (org-super-agenda-mode))

  ;; try verb package to serve REST/HTTP requests
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
;; }}}

;;{{{ Prog languages modes preferences

;; Common mapping
(map! :leader
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; Clojure
(after! cider
  (add-to-list 'exec-path "/home/va/.asdf/shims")
  (setq nrepl-use-ssh-fallback-for-remote-hosts t) ;; Cider should be able to connect to remote hosts using ssh
  (setq cider-default-repl-command "lein")
  (setq cider-eldoc-display-context-dependent-info t)
  (setq clojure-align-forms-automatically t)
  (map! :after clojure-mode :map clojure-mode-map :localleader ;; faster simpler workflow shortcuts
        ("f" #'consult-flycheck)
        ("m" #'cider-selector)
        ("T" #'projectile-toggle-between-implementation-and-test))
  (evil-make-intercept-map cider--debug-mode-map 'normal) ;; don't mess evil-mode with cider debug
  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps))

;; Let's try computer to be agressive while indenting clojure
(use-package aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))

;; Prettify clojure code
(defvar personal/clojure-prettify-alist '())

(add-to-list 'personal/clojure-prettify-alist '(">=" . ?≧))
(add-to-list 'personal/clojure-prettify-alist '("<=" . ?≦))
(add-to-list 'personal/clojure-prettify-alist '("fn" . ?ƒ))
(add-to-list 'personal/clojure-prettify-alist '("lambda" . ?λ))
(add-to-list 'personal/clojure-prettify-alist '("->"  . 10230))
(add-to-list 'personal/clojure-prettify-alist '("->>" . 10233))

(eval-after-load 'clojure-mode
  '(setq clojure--prettify-symbols-alist
         (append personal/clojure-prettify-alist
                 clojure--prettify-symbols-alist)))
;;}}}


;;; {{{ Block of custom functions and their bindings
(load! "abbrev.el")
(load! "custom.el")
;; bindings for custom.el
(global-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp)
;;}}}

;;{{{ Zetteldeft: return of emacsian Luhmann
(use-package! deft
  :custom
  (deft-extensions '("org" "md" "txt"))
  (deft-text-mode 'org-mode)
  (deft-directory "~/org/zetteln")
  (deft-default-extension "org")
  (deft-use-filename-as-title t))

(use-package zetteldeft
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

;;{{{ Better git blame
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
  (setq blamer-type 'both)
  )

;; to see saved but not committed things in all the files
(global-blamer-mode 1)
;;;;}}}

;;;{{{ Modeline optional stuff (can live without it)
(setq doom-modeline-github t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-persp-name t)
(setq doom-modeline-persp-icon nil)
(setq doom-modeline-icon nil)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-lsp nil)
(setq doom-modeline-percent-position nil)
(setq doom-modeline-project-detection 'project)
(setq doom-modeline-bar-width 2)
(setq doom-modeline--limited-width-p t)
;;;}}}

;;; {{{ topsy shows what function is shown partially
(require 'topsy)
(add-hook 'prog-mode-hook #'topsy-mode)
;;; }}}
(evil-set-initial-state 'deadgrep-mode 'emacs)

(use-package! lispyville
  :when (featurep! :editor evil)
  :hook (lispy-mode . lispyville-mode)
  :init
  (setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert))
  :config
  (lispyville-set-key-theme))

(keycast-tab-bar-mode)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not free-vars)
;; End:
