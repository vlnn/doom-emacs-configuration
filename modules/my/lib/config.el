;;; my/lib/config.el -*- lexical-binding: t; -*-
;; Helpers more than one module needs; loaded first, named without a module scope.

(defun +git-lines (&rest args)
  "Stdout lines of git ARGS in `default-directory', or nil when git fails; stderr never mixes in."
  (with-temp-buffer
    (when (zerop (apply #'call-process "git" nil (list t nil) nil args))
      (split-string (buffer-string) "\n" t))))

(defun +git-ok-p (&rest args)
  (zerop (apply #'call-process "git" nil nil nil args)))

(defun +read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun +git-common-dir (dir)
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (car (+git-lines "rev-parse" "--path-format=absolute" "--git-common-dir")))))

(defun +main-checkout (dir)
  "The repo DIR belongs to: its main checkout for a worktree, else DIR itself."
  (let ((common (+git-common-dir dir)))
    (if (and common (equal (file-name-nondirectory common) ".git"))
        (file-name-as-directory (file-name-directory common))
      dir)))

(defconst +justfile-regexp "\\`\\.?justfile\\'"
  "Names just reads as the justfile, matched in any case, as just does.")

(defun +justfiles (root)
  "ROOT's justfiles, each once under its name on disk, whatever its case."
  (let ((case-fold-search t))
    (and (file-directory-p root)
         (seq-filter (lambda (file) (string-match-p +justfile-regexp (file-name-nondirectory file)))
                     (directory-files root t directory-files-no-dot-files-regexp)))))

(defun +project-root ()
  (or (projectile-project-root) default-directory))
