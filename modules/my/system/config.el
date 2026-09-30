;;; my/system/config.el -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control
        dired-use-ls-dired nil)

  (when-let* ((fish (executable-find "fish")))
    (setq-default vterm-shell fish
                  explicit-shell-file-name fish))

  (when-let* ((bash (executable-find "bash")))
    (setq shell-file-name bash)))

;; machine-local values; git-ignored, see secrets.el.example
(load! "secrets" doom-user-dir t)

;; AI agents and external tools rewrite files under us; keep buffers fresh.
(global-auto-revert-mode 1)
