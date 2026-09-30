;;; my/vc/config.el -*- lexical-binding: t; -*-

(map! :leader :desc "Blame this line" "g l" #'why-this)

(after! magit
  (magit-todos-mode 1)
  (add-hook 'magit-mode-hook #'magit-delta-mode))

(defun +github-explorer--parent-buffer-name (name)
  (replace-regexp-in-string "[^/]+/$" "" name))

(defun +github-explorer/up ()
  (interactive)
  (let ((parent (+github-explorer--parent-buffer-name (buffer-name))))
    (if (get-buffer parent)
        (switch-to-buffer parent)
      (message "Parent buffer not found"))))

(use-package! github-explorer
  :commands (github-explorer)
  :init
  (map! :leader
        (:prefix ("G" . "github")
         :desc "Explore repo" "e" #'github-explorer))
  :config
  (map! :map github-explorer-mode-map
        :n "RET" #'github-explorer-at-point
        :n "SPC" #'github-explorer-at-point
        :n "d"   #'github-explorer-search
        :n "f"   #'github-explorer-find-file
        :n "-"   #'+github-explorer/up
        :n "DEL" #'+github-explorer/up))
