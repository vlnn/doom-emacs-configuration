;;; dirvish.el -*- lexical-binding: t; -*-

(after! dirvish
  (setq dirvish-attributes
        '(hl-line subtree-state nerd-icons file-size git-msg file-time)
        dirvish-subtree-state-style 'nerd
        dirvish-preview-dispatchers '(image gif video audio epub archive pdf)
        dirvish-default-layout      '(0 0.3 0.6)
        dirvish-header-line-format  '(:left (path) :right (free-space))
        dirvish-mode-line-format    '(:left (sort symlink) :right (omit yank index)))

  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group"
        dired-dwim-target t
        delete-by-moving-to-trash t
        dired-omit-files (rx (or (seq bol "." (not (any ".")))
                                 (seq bol ".." eol))))

  (map! :map dirvish-mode-map
        :n "TAB" #'dirvish-subtree-toggle
        :n "q"   #'dirvish-quit
        :n "a"   #'dirvish-dispatch
        :n "s"   #'dirvish-quicksort
        :n "y"   #'dirvish-yank
        :n "Y"   #'dirvish-yank-menu
        :n "N"   #'dirvish-narrow
        :n "H"   #'dirvish-history-jump
        :n "F"   #'dirvish-fd
        :n "h"   #'dired-up-directory
        :n "l"   #'dired-find-file
        :n "."   #'dired-omit-mode
        :n "/"   #'dirvish-fd-ask))
