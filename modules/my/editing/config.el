;;; my/editing/config.el -*- lexical-binding: t; -*-

(load! "completion")
(load! "evil")
(load! "keychords")
(load! "avy")

(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace"        "%" #'query-replace
      :desc "Undo abbrev"          "U" #'unexpand-abbrev
      :desc "Consult flycheck"     "F" #'consult-flycheck)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|()))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(use-package! demo-it
  :config
  (map! :map demo-it-mode-map "<f12>" #'demo-it-step))

(use-package! drag-stuff
  :defer t
  :init
  (map! "<M-up>"   #'drag-stuff-up
        "<M-down>" #'drag-stuff-down))

(use-package! expand-region
  :bind (:map evil-visual-state-map
         ("v" . er/expand-region)))

(use-package! super-save
  :config
  (setq super-save-auto-save-when-idle t)
  (super-save-mode 1))

(use-package! deadgrep
  :commands (deadgrep)
  :init
  (map! :leader :desc "Deadgrep" "s D" #'deadgrep))

(defun +projectile--without-vertico-sort (fn &rest args)
  (let ((vertico-sort-function nil))
    (apply fn args)))

(after! projectile
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-sort-order 'recentf)
  (advice-add 'projectile-switch-project :around #'+projectile--without-vertico-sort))
