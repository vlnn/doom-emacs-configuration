;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 18))
;; (setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(load-theme 'tsdh-light t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq deft-extensions '("org" "md" "txt"))
(setq deft-directory "~/Zettels")
(setq deft-recursive t)
(global-set-key [f12] 'deft)
(setq zetteldeft-title-prefix nil)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map ";" 'evil-forward-char)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "l" 'evil-previous-line)

;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)
(defun org-pretty-symbols-mode ()
  (push '("#+title: "        . "") prettify-symbols-alist)
  (push '("#+subtitle: "     . "") prettify-symbols-alist)
  (push '("* "               . "") prettify-symbols-alist)
  (push '("** "              . "") prettify-symbols-alist)
  (push '("*** "             . "") prettify-symbols-alist)
  (push '("**** "            . "") prettify-symbols-alist)
  (push '("***** "           . "") prettify-symbols-alist)
  (push '("#+author: "       . "- ") prettify-symbols-alist)
  (push '(":properties:"     . ":") prettify-symbols-alist)
  (push '("#+begin_src"      . "λ") prettify-symbols-alist)
  (push '("#+end_src"        . "⋱") prettify-symbols-alist)
  (push '("#+results:"       . "»") prettify-symbols-alist)
  (push '(":end:"            . "⋱") prettify-symbols-alist)
  (push '(":results:"        . "⋰") prettify-symbols-alist)
  (push '("#+name:"          . "-") prettify-symbols-alist)
  (push '("[ ]" .  "☐") prettify-symbols-alist)
  (push '("[X]" . "☑" ) prettify-symbols-alist)
  (push '("[-]" . "❍" ) prettify-symbols-alist)
  (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
  (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
  (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
  (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
  (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
  (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
  (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
  (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
  (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
  (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
  (push '("\\\\"             . "↩") prettify-symbols-alist)
  (prettify-symbols-mode t))
(require 'zetteldeft)
(map! :leader (:prefix-map ("z" "Zettels")
                :desc "New Zettel" "z" 'zetteldeft-new-file
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
                ))
