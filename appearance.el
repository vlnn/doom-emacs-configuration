;;; appearance.el -*- lexical-binding: t; -*-

(defvar +appearance-font-family "DepartureMono Nerd Font Propo")

(setq doom-font        (font-spec :family +appearance-font-family :size 13 :weight 'regular)
      doom-symbol-font (font-spec :family +appearance-font-family :size 13 :weight 'thin)
      doom-big-font    (font-spec :family +appearance-font-family :size 23 :weight 'regular))

(setq doom-theme 'doom-flatwhite)

(after! doom-themes
  (custom-set-faces!
    '(font-lock-keyword-face :weight regular))
  (doom/reload-font))

(blink-cursor-mode 1)
(set-cursor-color "dark blue")
(setq-default line-spacing 1)

(custom-set-faces!
  '(hl-line :background "#d5f7d5"))

(mouse-avoidance-mode 'animate)
(setq mouse-avoidance-threshold 0.5)

(setq display-line-numbers-type t)
(setq word-wrap nil)

(setq frame-resize-pixelwise t
      window-resize-pixelwise t
      frame-inhibit-implied-resize t)

(use-package! ultra-scroll
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))
