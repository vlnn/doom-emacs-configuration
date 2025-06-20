(use-package! org-roam
  :init
  (map! :leader
        :prefix "j"
        :desc "org-roam" "j" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "F" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "jethro/org-capture-slipbox" "<tab>" #'jethro/org-capture-slipbox
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/Documents/org/")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t)
  :config
  (org-roam-db-autosync-mode +1)
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)))
  (defun jethro/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  )
