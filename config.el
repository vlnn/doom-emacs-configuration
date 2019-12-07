;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; workflow configuration
(evil-snipe-override-mode +1)

;;; booting and starting up configs
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;; files related configs
(setq org-agenda-files (list "~/devlog.org")
      projectile-project-search-path '("~/src"))

;;; themes and styles related configs
;(setq doom-theme 'doom-one)
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 20)
      doom-big-font (font-spec :family "Fantasque Sans Mono" :size 30)
      font-lock-comment-face 'italic)
(setq display-line-numbers-type 'relative)
(setq treemacs-directory-face 'bold
      treemacs-file-face 'default)
