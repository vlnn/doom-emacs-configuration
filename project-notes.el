;;; ../../src/doom-emacs-configuration/project-notes.el -*- lexical-binding: t; -*-

(use-package! denote
  :config
  (setq denote-directory (expand-file-name "~/org/denote/"))
  (setq denote-known-keywords '("emacs" "journal" "project" "idea"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil) ; org by default
  (setq denote-prompts '(title keywords))

  ;; Helper functions for project note paths
  (defun denote-project-notes-directory (project-name)
    "Get the directory path for PROJECT-NAME notes."
    (expand-file-name (format "projects/%s" (denote-sluggify-title project-name))
                      denote-directory))

  (defun denote-ensure-project-directory (project-name)
    "Ensure the directory for PROJECT-NAME exists."
    (let ((dir (denote-project-notes-directory project-name)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      dir))

  ;; Note creation functions
  (defun denote-generate-project-note-filename (project-name note-title)
    "Generate filename for a project note with PROJECT-NAME and NOTE-TITLE."
    (format "%s--%s__project.org"
            (format-time-string "%Y%m%dT%H%M%S")
            (denote-sluggify-title note-title)))

  (defun denote-create-project-note-frontmatter (project-name note-title)
    "Create frontmatter for a project note."
    (format "#+title: %s - %s\n#+filetags: :project:%s:\n#+date: %s\n\n"
            project-name
            note-title
            (denote-sluggify-title project-name)
            (format-time-string "%Y-%m-%d")))

  ;; Note discovery functions
  (defun denote-list-project-notes (project-name)
    "List all note files for PROJECT-NAME."
    (let ((dir (denote-project-notes-directory project-name)))
      (when (file-directory-p dir)
        (directory-files dir t "\\.org$"))))

  (defun denote-extract-note-title (filepath)
    "Extract the note title from FILEPATH."
    (with-temp-buffer
      (insert-file-contents filepath nil 0 200)
      (if (re-search-forward "^#\\+title: .*? - \\(.+\\)$" nil t)
          (match-string 1)
        (file-name-base filepath))))

  (defun denote-format-note-for-selection (filepath project-name)
    "Format FILEPATH for selection display."
    (let* ((title (denote-extract-note-title filepath))
           (mtime (file-attribute-modification-time (file-attributes filepath)))
           (date (format-time-string "%Y-%m-%d" mtime)))
      (format "%-40s  [%s]" title date)))

  ;; Selection interface
  (defun denote-select-project-note (project-name)
    "Select a note from PROJECT-NAME's notes."
    (let ((notes (denote-list-project-notes project-name)))
      (cond
       ((null notes) nil)
       ((= (length notes) 1) (car notes))
       (t (denote-prompt-for-note-selection notes project-name)))))

  (defun denote-prompt-for-note-selection (notes project-name)
    "Prompt user to select from NOTES list."
    (let* ((choices (mapcar (lambda (note)
                              (cons (denote-format-note-for-selection note project-name)
                                    note))
                            notes))
           (selection (completing-read
                       (format "Select note for %s: " project-name)
                       choices nil t)))
      (cdr (assoc selection choices))))

  ;; New note creation
  (defun denote-create-new-project-note (project-name)
    "Create a new note for PROJECT-NAME."
    (let* ((note-title (read-string "Note title: "))
           (dir (denote-ensure-project-directory project-name))
           (filename (denote-generate-project-note-filename project-name note-title))
           (filepath (expand-file-name filename dir)))
      (cons filepath note-title)))

  ;; Popup management
  (defun denote-setup-popup-keybindings ()
    "Setup keybindings for popup buffer."
    (local-set-key (kbd "q") #'denote-save-and-close-popup)
    (evil-local-set-key 'normal (kbd "q") #'denote-save-and-close-popup))

  (defun denote-save-and-close-popup ()
    "Save buffer and close popup."
    (interactive)
    (save-buffer)
    (+popup/close))

  (defun denote-open-project-popup (buffer)
    "Open BUFFER in a popup window and focus it."
    (let ((popup-window (+popup-buffer buffer '((side . right) (size . 0.4)))))
      (when popup-window
        (select-window popup-window))))

  (defun denote-setup-project-note-buffer (buffer project-name note-title)
    "Setup BUFFER for a project note."
    (with-current-buffer buffer
      (when (= (point-max) 1)
        (insert (denote-create-project-note-frontmatter project-name note-title)))
      (denote-setup-popup-keybindings)))

  ;; Main interactive functions
  (defun denote-project-notes ()
    "Open or create a project note for current project."
    (interactive)
    (if-let ((project-name (projectile-project-name)))
        (denote-handle-project-note-selection project-name)
      (message "Not in a projectile project")))

  (defun denote-handle-project-note-selection (project-name)
    "Handle note selection or creation for PROJECT-NAME."
    (let* ((existing-notes (denote-list-project-notes project-name))
           (choice (denote-prompt-note-action existing-notes)))
      (cond
       ((eq choice 'new)
        (denote-open-new-project-note project-name))
       ((stringp choice)
        (denote-open-existing-project-note choice))
       (t (message "Cancelled")))))

  (defun denote-prompt-note-action (existing-notes)
    "Prompt for action with EXISTING-NOTES."
    (if existing-notes
        (let ((choices (append '(("+ Create new note" . new))
                               (mapcar (lambda (note)
                                         (cons (denote-format-note-for-selection note nil)
                                               note))
                                       existing-notes))))
          (cdr (assoc (completing-read "Select note: " choices nil t) choices)))
      'new))

  (defun denote-open-new-project-note (project-name)
    "Create and open a new note for PROJECT-NAME."
    (let* ((result (denote-create-new-project-note project-name))
           (filepath (car result))
           (note-title (cdr result))
           (buffer (find-file-noselect filepath)))
      (denote-setup-project-note-buffer buffer project-name note-title)
      (denote-open-project-popup buffer)))

  (defun denote-open-existing-project-note (filepath)
    "Open existing note at FILEPATH."
    (let ((buffer (find-file-noselect filepath)))
      (with-current-buffer buffer
        (denote-setup-popup-keybindings))
      (denote-open-project-popup buffer)))

  ;; Keybindings
  (map! :leader
        (:prefix-map ("n d" . "denote")
         :desc "Create note" "n" #'denote
         :desc "Open or create" "o" #'denote-open-or-create
         :desc "Find file" "f" #'denote-find-file
         :desc "Find by keyword" "k" #'denote-find-file-by-keyword
         :desc "Insert link" "l" #'denote-link
         :desc "Backlinks" "b" #'denote-backlinks
         :desc "Project notes" "p" #'denote-project-notes))

  ;; Add to projectile menu
  (map! :leader
        :prefix "p"
        :desc "Project notes" "n" #'denote-project-notes))
