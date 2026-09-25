;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

;; I don't like to comment out a block of lisp with ;
(defmacro comment (&rest _body)
  "Comment out one or more s-expressions."
  nil)

;;; Load order:
;;;   macos        — must run before anything reads shell-file-name
;;;   secrets      — early so later configs can reference it
;;;   appearance   — fonts/theme/scroll
;;;   completion   — corfu/abbrev/indent
;;;   evil         — keymap + REPL initial states
;;;   keychords    — depends on avy/frog-jump-buffer being declared
;;;   avy          — loads avy-functions.el internally
;;;   lsp          — flycheck + jsonian wired in
;;;   python       — python-mode tweaks
;;;   dape         — debugging
;;;   dirvish      — file manager
;;;   notes        — denote
;;;   ai           — aider / ai-code / gptel / mindstream
;;;   forth        — forth-mode + dumb-jump rules
;;;   apl          — gnu-apl-mode + ride-apl.el, mirrors the cider setup
;;;   casual       — transient menus across modes
;;;   snippets     — yas helpers used by snippet files
(load! "macos.el")
(load! "secrets.el" nil t)  ; tolerated missing — see secrets.el.example
(load! "appearance.el")
(load! "completion.el")
(load! "evil.el")
(load! "keychords.el")
(load! "avy.el")
(load! "lsp.el")
(load! "python.el")
(load! "dape.el")
(load! "dirvish.el")
(load! "notes.el")
;(load! "ai.el")
(load! "forth.el")
(load! "apl.el")
(load! "casual.el")
(load! "snippets.el")

;;; Misc leader bindings
(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace"        "%" #'query-replace
      :desc "Undo abbrev"          "U" #'unexpand-abbrev
      :desc "Consult flycheck"     "F" #'consult-flycheck
      :desc "Blame this line"      "g l" #'why-this)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|()))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;;; Tiny package configs that don't earn their own file

(use-package! demo-it
  :config
  (map! :map demo-it-mode-map "<f12>" #'demo-it-step))

(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"   #'drag-stuff-up
        "<M-down>" #'drag-stuff-down))

(use-package! mini-ontop
  :ensure t
  :config
  (mini-ontop-mode 1)
  (setq mini-ontop-lines 40))

(use-package! expand-region
  :bind (:map evil-visual-state-map
         ("v" . er/expand-region)))

(use-package! casual)

(use-package! ob-duckdb)

(use-package! github-explorer
  :commands (github-explorer)
  :init
  (map! :leader
        (:prefix ("G" . "github")
         :desc "Explore repo" "e" #'github-explorer))
  :config
  (defun +github-explorer/up ()
    (interactive)
    (let* ((name (buffer-name))
           (parent (replace-regexp-in-string "[^/]+/$" "" name)))
      (if (get-buffer parent)
          (switch-to-buffer parent)
        (message "Parent buffer not found"))))

  (map! :map github-explorer-mode-map
        :n "RET" #'github-explorer-at-point
        :n "SPC" #'github-explorer-at-point
        :n "d"   #'github-explorer-search
        :n "f"   #'github-explorer-find-file
        :n "-"   #'+github-explorer/up
        :n "DEL" #'+github-explorer/up))

(use-package! elfeed-score
  :ensure t
  :config
  (elfeed-score-enable)
  (define-key elfeed-search-mode-map "=" elfeed-score-map))

(after! projectile
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf))

(after! (vertico projectile)
  (defun +projectile-no-sort (fn &rest args)
    (let ((vertico-sort-function nil))
      (apply fn args)))
  (advice-add 'projectile-switch-project :around #'+projectile-no-sort))

(after! plantuml-mode
  (setq plantuml-default-exec-mode 'executable
        plantuml-executable-path   "plantuml"
        org-plantuml-executable-path "plantuml"
        org-plantuml-exec-mode      'executable)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

