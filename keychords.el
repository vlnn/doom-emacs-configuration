;;; keychords.el -*- lexical-binding: t; -*-

(key-chord-mode 1)

(after! key-chord
  (key-chord-define-global "jk" 'avy-goto-char-timer)
  (key-chord-define-global "bn" 'frog-jump-buffer)
  (key-chord-define-global "ji" 'evil-avy-goto-line)
  (key-chord-define-global "78" 'sp-beginning-of-previous-sexp)
  (key-chord-define-global "89" 'sp-beginning-of-sexp)
  (key-chord-define-global "90" 'sp-end-of-sexp)
  (key-chord-define-global "0-" 'sp-end-of-next-sexp)
  (key-chord-define-global "sd" 'basic-save-buffer)
  (key-chord-define-global "mn" 'dap-hydra)
  (key-chord-define-global "j;" 'execute-extended-command))
