;;; org-mode.el -*- lexical-binding: t; -*-

(defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 2)))
         (hd (condition-case nil
                 (car (org-refile-get-location "Tag" nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading))
  (end-of-line))

(after! org
  :config
  (setq org-directory "~/Documents/org/"
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files (list "archives.org" "areas.org" "projects.org" "resources.org"))
  (setq org-log-done 'time
        org-log-reschedule 'time
        org-log-into-drawer t
        org-startup-indented t
        org-startup-truncated nil
        org-reverse-note-order t  ; new stuff should be higher in the files
        org-id-track-globally t)
  (add-to-list 'org-global-properties
         '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 1d 2d 1w 2w 1m"))
  (setq org-tags-column 80
        org-todo-keyword-faces
         '(("TODO" . "khaki4")
           ("NOW" . "orange")
           ("LATER" . "khaki2")
           ("PROJ" . "green")
           ("ACT" . "tan3")
           ("AREA" . "tan3")
           ("TOREAD" . "tan2")
           ("QUESTION" . "tan1")
           ("ANSWER" . "dark green")
           ("IDEA" . "blue")
           ("DONE" . "green4")
           ("KILL" . "green3"))
        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "REPEAT(r)" "NEXT(n)" "LATER(l)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)") ; P of PARA
                            (sequence "AREA(a)" "PROGRESS(p)" "|" "KILL(k)")                                                            ; A of PARA
                            (sequence "SOURCE(s)" "TOREAD(t)" "READING(r)" "IDEA(i)" "|" "KILL(k)" "FINISHED(d)")                                   ; R of PARA
                            (sequence "QUESTION(Q)" "IDEA(i)" "|" "ANSWER(a)" "KILL(k)"))                                              ; A of PARA

       cfw:org-overwrite-default-keybinding t)

  (setq org-capture-templates
        '(("p" "Projects")
          ("pw" "Projects at work" entry (file+headline "projects.org" "Work") "* TODO %? :work:" :prepend t)
          ("pp" "Personal Projects Todo" entry (file+headline "projects.org" "Personal") "* TODO %? :personal:" :prepend t)
          ("pP" "Personal Projects" entry (file+headline "projects.org" "Personal") "* PROJ %? :personal:" :prepend t)
          ("a" "Areas todo" entry (file+function "areas.org" org-ask-location) "*** TODO %?" :prepend t)
          ("A" "Areas" entry (file "areas.org") "* AREA %?" :prepend t)
          ("r" "Resources item" entry (file+function "resources.org" org-ask-location) "* TOREAD %?" :prepend t)
          ("R" "Resources source" entry (file "resources.org") "* SOURCE %?" :prepend t)
          ("Q" "I have a Question for archives!" entry (file "archives.org") "* QUESTION %?" :prepend t)
          ("I" "I have an Idea for archives!" entry (file "archives.org") "* IDEA %?" :prepend t)))

 (defun org-summary-todo (n-done n-not-done)
   "Switch entry to DONE when all subentries are done, to TODO otherwise."
   (let (org-log-done org-todo-log-states)   ; turn off logging
     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

 (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
 (add-hook 'auto-save-hook 'org-save-all-org-buffers)

 (setq org-agenda-span 5
       org-agenda-start-day "-1d"
       org-agenda-start-with-clockreport-mode t
       org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 2 :fileskip0 t :step day) ; no empty records in the agenda
       org-agenda-todo-keyword-format "%s"
       org-agenda-prefix-format '((agenda . "%i %-15:c%?-15t%s %b")
                                  (timeline . "  % s")
                                  (todo . " %i %-12:c")
                                  (tags . " %i %-12:c")
                                  (search . " %i %-12:c"))
       org-agenda-start-with-log-mode t
       org-agenda-start-with-follow-mode t
       org-agenda-include-diary t
       org-agenda-compact-blocks t
       org-agenda-show-future-repeats nil
       org-habit-show-all-today t
       org-habit-show-done-always-green t
       org-habit-preceding-days 7
       org-habit-following-days 2
       org-use-property-inheritance t))

(use-package! org-agenda
  :custom
   (org-agenda-custom-commands
    '(("A" "Priority #A tasks" agenda ""
       ((org-agenda-ndays 1)
        (org-agenda-skip-function
         '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]"))))
      ("B" "Priority #B tasks" agenda ""
       ((org-agenda-ndays 1)
        (org-agenda-skip-function
         '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#B\\]"))))
      ("C" "Priority #C tasks" agenda ""
       ((org-agenda-ndays 1)
        (org-agenda-skip-function
         '(org-agenda-skip-entry-if 'notregexp "\\=.*\\[#C\\]"))))
      ("S" "Super view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))))
      ("c" "aCtivities" todo "ACT")
      ("t" "To do" todo "TODO")
      ("T" "To do #A" todo "TODO [#A]|HOLD [#A]|WAIT [#A]")
      ("i" "In progress" todo "PROGRESS|WAIT")
      ("h" "On hold" todo "HOLD")
      ("d" "Done" todo "DONE|KILL"))))

(after! org
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil
        super-save-all-buffers t))

; deft is being used for fast search across all org files with SPC n d
(setq deft-directory "~/Documents/org")
(setq deft-default-extension "org")