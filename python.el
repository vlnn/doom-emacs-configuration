;;; python.el -*- lexical-binding: t; -*-

(after! python
  (defun my/set-python-interpreter ()
    "Set Python interpreter based on current Poetry project"
    (when-let* ((project-root (project-root (project-current)))
                (poetry-env (shell-command-to-string
                             (format "cd %s && poetry env info --path" project-root))))
      (setq python-shell-interpreter
            (concat (string-trim poetry-env) "/bin/python -i"))))

  (setq python-shell-send-buffer-function #'python-shell-send-file)
  (poetry-tracking-mode)

  (add-hook 'project-switch-project-hook #'my/set-python-interpreter))

(map! :after python
      :map (python-mode-map python-ts-mode-map)
      :localleader
      (:prefix ("e" . "eval")
       :desc "eval statement"  "e" #'python-shell-send-statement
       :desc "eval function"  "f" #'python-shell-send-defun
       :desc "eval file"      "F" #'python-shell-send-file
       :desc "eval buffer"    "r" #'python-shell-send-buffer
       :desc "eval region"    "r" #'python-shell-send-region)
      (:prefix ("p" . "pipenv")
       :desc "run"       "r" #'pipenv-run))
