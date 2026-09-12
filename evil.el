;;; evil.el -*- lexical-binding: t; -*-

;; default undo settings are too aggregative to my likings
;; NB: Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; how many times did you SPC m e e
;; just to see result of some internal sexp evaluation, not the whole sexp you're in?
;; Too many, not anymore!
(setq evil-move-beyond-eol t
      evil-move-cursor-back nil
      evil-highlight-closing-paren-at-point-states nil)

;; Move cursor with 'jkl;', not default evil 'hjkl'
(setq evil-snipe-override-evil-repeat-keys nil)

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map "j"  'evil-backward-char)
  (define-key evil-motion-state-map "\;" 'evil-forward-char)
  (define-key evil-motion-state-map "k"  'evil-next-visual-line)
  (define-key evil-motion-state-map "l"  'evil-previous-visual-line))

(map! :map evil-window-map
      "j"     #'evil-window-left
      "k"     #'evil-window-down
      "l"     #'evil-window-up
      ";"     #'evil-window-right
      "C-h"   #'+evil/window-move-left
      "C-k"   #'+evil/window-move-down
      "C-\;"  #'+evil/window-move-right)

(map! :map (minibuffer-mode-map
            ivy-minibuffer-map
            vertico-map)
      :g "C-k" #'next-line
      :g "C-l" #'previous-line)

;; REPLs should start in insert mode
(after! evil
  (dolist (mode '(dape-repl-mode
                  ride-apl-repl-mode
                  comint-mode
                  inferior-python-mode
                  eshell-mode
                  term-mode))
    (evil-set-initial-state mode 'insert)))

(after! winum
  (setq winum-scope 'visible
        winum-auto-setup-mode-line t))

(after! evil-org
  (map! :map evil-org-mode-map
        :nvm "k"      #'evil-next-visual-line
        :nvm "l"      #'evil-previous-visual-line
        :nvm "<down>" #'evil-next-visual-line
        :nvm "<up>"   #'evil-previous-visual-line))
