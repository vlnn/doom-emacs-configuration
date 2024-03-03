;;; codeium.el -*- lexical-binding: t; -*-

(defun my/cape-codeium (&optional interactive)
  "Allow codeium capf to be run by itself"
  (interactive (list t))
  (when interactive
    ;; if also testing copilot, clear their overlay before showing capf popup
    (when (bound-and-true-p copilot-mode) (copilot-clear-overlay))
    (cape-interactive #'codeium-completion-at-point)))
(map! :leader
      :desc "Try AI" "A" #'my/cape-codeium)
(keymap-global-set "C-c a i" #'my/cape-codeium)

(use-package! codeium
  :config
  (add-hook! prog-mode (add-hook 'completion-at-point-functions #'codeium-completion-at-point 100 t)))
