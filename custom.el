;;; custom.el -*- lexical-binding: t; -*-
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block (nil :foreground nil :inherit 'fixed-pitch))
 '(org-checkbox (nil :inherit 'fixed-pitch))
 '(org-code (nil :inherit '(shadow fixed-pitch)))
 '(org-document-title ((t (:height 1.3))))
 '(org-level-1 ((t (:inherit outline-1 :weight extra-bold :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.12))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.09))))
 '(org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.06))))
 '(org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.03))))
 '(org-level-7 ((t (:inherit outline-7 :weight semi-bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight semi-bold))))
 '(org-link ((t (:inherit outline-4 :weight bold :height 1.09))))
 '(org-meta-line (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-special-keyword (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-table (nil :inherit '(shadow fixed-pitch)))
 '(org-verbatim (nil :inherit '(shadow fixed-pitch))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-status-emoji aggressive-indent flycheck-clj-kondo))
 '(reverse-im-input-methods '("ukrainian-computer")))
