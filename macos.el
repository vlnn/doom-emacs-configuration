;;; macos.el -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control
        dired-use-ls-dired nil)

  (when-let* ((fish (executable-find "fish")))
    (setq-default vterm-shell fish
                  explicit-shell-file-name fish))

  (when-let* ((bash (executable-find "bash")))
    (setq shell-file-name bash)))
