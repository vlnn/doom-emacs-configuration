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
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 19))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the theme I'm using now:
(setq doom-theme 'doom-flatwhite)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Org- and zettel- related configuration
(after! org
  (setq org-directory "~/org/")
  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)" "CANCELLED(c)"))))
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "#7c7c75" :weight normal :underline t)
          ("DOING" :foreground "#9f7efe" :weight normal :underline t)
          ("DONE" :foreground "#50a14f" :weight normal :underline t)
          ("CANCELLED" :foreground "#ff6480" :weight normal :underline t))
        ))

(after! org
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
    '(org-checkbox nil :inherit 'fixed-pitch)))
(setq deft-extensions '("org" "md" "txt"))
(setq deft-directory "~/Zettels")
(setq deft-recursive t)
(global-set-key [f12] 'deft)
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
               :desc "Rename Zettel" "r" 'zetteldeft-file-rename
               ))

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

;; Use mmds that is installed using brew
(setq ob-mermaid-cli-path "/home/linuxbrew/.linuxbrew/Cellar/mermaid-cli/8.11.0.reinstall/bin/mmdc")
