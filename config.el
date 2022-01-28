;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; While I'm not hiding, I see no benefit in showing those things to the wide internet.
;; (setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

;; Fantasque is great font, so let's use it where possible
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the theme I'm using now:
;;(setq doom-theme 'doom-flatwhite)
;;(setq doom-theme 'doom-tomorrow-day)
(setq doom-theme 'doom-homage-white)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

;; one big file for refile
(setq org-default-notes-file "~/org/refile.org")

;; Org- and zettel- related configuration
(after! org
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

  (custom-set-faces!
    '(org-document-title :height 1.3)
    '(org-level-1 :inherit outline-1 :weight extra-bold :height 1.4)
    '(org-level-2 :inherit outline-2 :weight bold :height 1.15)
    '(org-level-3 :inherit outline-3 :weight bold :height 1.12)
    '(org-level-4 :inherit outline-4 :weight bold :height 1.09)
    '(org-link :inherit outline-4 :weight bold :height 1.09)
    '(org-level-5 :inherit outline-5 :weight semi-bold :height 1.06)
    '(org-level-6 :inherit outline-6 :weight semi-bold :height 1.03)
    '(org-level-7 :inherit outline-7 :weight semi-bold)
    '(org-level-8 :inherit outline-8 :weight semi-bold)
    ;; Ensure that anything that should be fixed-pitch in org buffers appears that
    ;; way
    '(org-block nil :foreground nil :inherit 'fixed-pitch)
    '(org-code nil   :inherit '(shadow fixed-pitch))
    '(org-table nil   :inherit '(shadow fixed-pitch))
    '(org-verbatim nil :inherit '(shadow fixed-pitch))
    '(org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-checkbox nil :inherit 'fixed-pitch))
  (setq org-startup-with-inline-images t)

  ;; org capture things
  ;; another global capture mode key (default is SPC X)
  (global-set-key (kbd "C-c c") 'org-capture)

  ;; org refile by default should be reversed so latest tasks are on top of the target filek
  (map! :leader (:prefix-map ("r" . "refile")
                 :desc "refile to file" "r" 'org-refile-reverse))

  ;; temporarily workaround startup problem of org :(
  (defun native-comp-available-p () nil)

  ;; Every split is for some buffer
  (defadvice! prompt-for-buffer (&rest _)
    :after '(evil-window-split evil-window-vsplit)
    (consult-buffer))

  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
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
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

                                        ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

                                        ; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

                                        ; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

                                        ; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

                                        ; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
                                        ; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
                                        ; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings -- shamelessly stolen from doc.norang.ca/org-mode.html
                                        ; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; zettel-deft setup
(setq deft-extensions '("org" "md" "txt"))
(setq deft-directory "~/Zettels")
(setq deft-recursive t)
(setq zetteldeft-title-prefix nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; autosave things whenever you change the the focus
(super-save-mode +1)

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

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; First change evil-repeat keys...
(setq evil-snipe-override-evil-repeat-keys nil)
;; and then use jkl; instead of hjkl... It's stupid how hard it is to re-map ;!
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map ";" 'evil-forward-char)
  (define-key evil-motion-state-map "k" 'evil-next-line)
  (define-key evil-motion-state-map "l" 'evil-previous-line))

;; Don't fold org files
(setq org-startup-folded "showall")

;; Mappings for Zettel flow (which I tend to return to form time to time)
(map! :leader (:prefix-map ("d" . "Zettels")
               :desc "New Zettel" "d" 'zetteldeft-new-file
               :desc "Link Zettel" "l" 'zetteldeft-find-file-full-title-insert
               :desc "Branch Zettel" "b" 'zetteldeft-new-file-and-link
               :desc "Search At Cursor" "c" 'zetteldeft-search-at-point
               :desc "Search Link" "s" 'zetteldeft-avy-link-search
               :desc "Search Tag" "t" 'zetteldeft-avy-tag-search
               :desc "Open Link" "o" 'zetteldeft-follow-link
               :desc "Find Zettel" "f" 'zetteldeft-find-file
               :desc "Search text" "S" 'zetteldeft--search-global
               :desc "Build graph from note" "g" 'zetteldeft-org-graph-note
               :desc "Link Tagged Zettels" "L" 'zetteldeft-insert-list-links
               :desc "Open File In Other Window" "O" 'deft-open-file-other-window
               :desc "Count number of words" "x" 'zetteldeft-count-words
               :desc "Rename Zettel" "r" 'zetteldeft-file-rename))

;; Mappings for things I've added instead of finding how to do it correctly

(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "h"     #'evil-window-left
      "k"     #'evil-window-down
      "l"       #'evil-window-up
      ";"    #'evil-window-right
      ;; Swapping windows
      "C-h"       #'+evil/window-move-left
      "C-k"       #'+evil/window-move-down
      "C-l"         #'+evil/window-move-up
      "C-;"      #'+evil/window-move-right)


;; TAB should work both for line indent and autocompletion
(setq-default c-basic-offset tab-width)
(setq tab-always-indent 'complete)

;; Set up two-language configuration
;; Я користуюся вбудованою підтримкою української мови
(use-package reverse-im
  :ensure t
  :config
  (reverse-im-mode t))

;; I also want to be able to run clojure code inside org
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(setq cider-jack-in-default 'lein)
(setq cider-allow-jack-in-without-project t)
(setq clojure-align-forms-automatically t)
(map! :leader (:prefix-map ("c" . "code")
               :desc "Format clojure" "f" 'cider-format-buffer))

;; And let's see if repl size can be defaulted to something better
(set-popup-rule! "^\\*cider-repl" :side 'bottom :height 0.3 :quit nil)

;; Run scheme using geiser using racket
(setq geiser-active-implementations '(racket))

;; Use sbcl with sly
(after! sly
  (load (expand-file-name "~/.roswell/helper.el"))
  (setq lisp-mode-hook 'sly-editing-mode))

;; I want to read software development-related (mostly emacs-related) news from emacs
(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)
  (setq elfeed-search-filter "@1-month-ago +unread"
        elfeed-search-title-min-width 80
        elfeed-show-entry-switch #'pop-to-buffer
        elfeed-show-entry-delete #'+rss/delete-pane
        shr-max-image-proportion 0.8
        rmh-elfeed-org-files (list "~/org/elfeed.org"))
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update))

;; Reading epub files
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))

;; I want info to be open at the right, not at the bottom
(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; Get vsplits automatically golden-ration'ed
(use-package! golden-ratio
  :after-call pre-command-hook
  :config
  (golden-ratio-mode +1)
  (remove-hook 'window-configuration-change-hook #'golden-ratio)
  (add-to-list 'golden-ratio-exclude-buffer-regexp "^\\*cider-repl")
  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-hook 'doom-switch-window-hook #'golden-ratio))

;; Use mmds that is installed using brew
(setq ob-mermaid-cli-path "/home/linuxbrew/.linuxbrew/Cellar/mermaid-cli/8.11.0.reinstall/bin/mmdc")

;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
(use-package avy
  :bind* (("C-j" . avy-goto-char-timer)))

;; I would like org clock to store info between emacs restarts
(require 'org-clock)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; Every file should have at least one new line in the end
(setq require-final-newline t)

;; Let's try computer to be agressive while indenting clojure
(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode))
