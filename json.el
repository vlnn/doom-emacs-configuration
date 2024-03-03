;;; json.el -*- lexical-binding: t; -*-

;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))
;; To disable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))
