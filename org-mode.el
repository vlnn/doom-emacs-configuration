(defun org-journal-buffer-p ()
  "Check if current buffer is an org-journal file."
  (and (derived-mode-p 'org-mode)
       (boundp 'org-journal-dir)
       (string-match-p (regexp-quote (expand-file-name org-journal-dir)) 
                       (or (buffer-file-name) ""))))

(defun schedule-unscheduled-todos ()
  "Schedule all TODO items without existing schedule for today."
  (save-excursion
    (goto-char (point-min))
    (let ((todo-regex (concat "^\\*+ \\(" (mapconcat 'regexp-quote org-not-done-keywords "\\|") "\\) ")))
      (while (re-search-forward todo-regex nil t)
        (let ((element (org-element-at-point)))
          (when (and (eq (org-element-type element) 'headline)
                     (not (org-get-scheduled-time (point))))
            (org-schedule nil (format-time-string "%Y-%m-%d"))))))))

(defun auto-schedule-journal-todos ()
  "Auto-schedule TODOs in journal files when saving."
  (when (org-journal-buffer-p)
    (schedule-unscheduled-todos)))

(defun ensure-journal-in-agenda ()
  "Add journal directory to agenda files if not already present."
  (when (and (boundp 'org-agenda-files)
             (boundp 'org-journal-dir)
             (not (member (expand-file-name org-journal-dir) 
                          (mapcar #'expand-file-name org-agenda-files))))
    (add-to-list 'org-agenda-files org-journal-dir)))


(add-hook 'before-save-hook 'auto-schedule-journal-todos)

(after! org
  (setq org-agenda-sorting-strategy 
        '((agenda time-up priority-down category-keep)
          (todo scheduled-up deadline-up priority-down timestamp-down category-keep)
          (tags scheduled-up deadline-up priority-down timestamp-down category-keep)  
          (search scheduled-up deadline-up priority-down timestamp-down category-keep)))
  (setq org-startup-folded 'showeverything
        org-journal-hide-entries-p nil
        org-directory "~/Documents/org/"
        org-journal-dir "~/Documents/org/areas/journal"
        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)"
                             "|" "DONE(d)" "KILL(k)")
                            (sequence "WTF(?)" "|" "TIL(.)")
                            (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))
  (setq org-todo-keyword-faces
        '(("KILL" . (:background "#e0e2e8" :weight bold))
          ("TODO" . (:background "#c5e7ef" :weight bold))
          ("DONE" . (:background "#d5d2d8" :weight bold)))))

(after! org-journal
  (setq org-journal-enable-agenda-integration t
        org-journal-file-format "%Y%m%d.org"
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"PROJ\"|TODO=\"LOOP\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\"|TODO=\"IDEA\"|TODO=\"WTF\"")
  (ensure-journal-in-agenda))

(defun my/org-journal-new-todo (&optional just-goto)
  "Create a new TODO item in today's org-journal entry.
With prefix argument JUST-GOTO, only go to today's entry without adding TODO."
  (interactive "P")
  (org-journal-new-entry t)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert "** TODO ")
  (save-buffer))

;; Keybinding configuration
(map! :leader
      :prefix "n j"
      :desc "New todo in journal" "t" #'my/org-journal-new-todo)



(after! doom-themes
  (custom-set-faces!
    '(org-level-1 :weight medium)
    '(org-level-2 :weight medium)
    '(org-level-3 :weight medium)
    '(org-level-4 :weight regular))
  (doom/reload-font))
