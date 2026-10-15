;;; my/lua/busted.el -*- lexical-binding: t; -*-

(defvar +lua-busted-program "busted")

(defconst +lua--test-name-regexp "\\_<it[ \t]*([ \t]*\\([\"']\\)\\(.*?\\)\\1")

(defun +lua--lua-pattern-quote (string)
  (replace-regexp-in-string "[]^$()%.[*+?-]" "%\\&" string t))

(defun +lua--nearest-test-name ()
  (save-excursion
    (end-of-line)
    (when (re-search-backward +lua--test-name-regexp nil t)
      (match-string-no-properties 2))))

(defun +lua--spec-file-p (file)
  (string-suffix-p "_spec.lua" file))

(defun +lua--spec-candidates (file root)
  (let* ((relative (file-relative-name (file-name-sans-extension file) root))
         (module (replace-regexp-in-string "\\`\\(?:src\\|lua\\)/" "" relative)))
    (list (concat (file-name-sans-extension file) "_spec.lua")
          (expand-file-name (concat "spec/" module "_spec.lua") root))))

(defun +lua--spec-file-for (file root exists-p)
  (if (+lua--spec-file-p file)
      file
    (seq-find exists-p (+lua--spec-candidates file root))))

(defun +lua--busted-command (&rest args)
  (mapconcat #'shell-quote-argument (cons +lua-busted-program args) " "))

(defun +lua--current-spec-file ()
  (let* ((file (or (buffer-file-name) (user-error "Buffer is not visiting a file")))
         (root (+lua--project-root))
         (spec (or (+lua--spec-file-for file root #'file-exists-p)
                   (user-error "No busted spec for %s" (file-name-nondirectory file)))))
    (file-relative-name spec root)))

(defun +lua--run-busted (&rest args)
  (let ((default-directory (+lua--project-root)))
    (compile (apply #'+lua--busted-command args))))

(defun +lua/test-project ()
  "Run every busted spec in the project."
  (interactive)
  (+lua--run-busted))

(defun +lua/test-file ()
  "Run the busted spec for this buffer."
  (interactive)
  (+lua--run-busted (+lua--current-spec-file)))

(defun +lua/test-at-point ()
  "Run the busted test before point."
  (interactive)
  (let ((name (or (+lua--nearest-test-name)
                  (user-error "No busted test before point"))))
    (+lua--run-busted (concat "--filter=" (+lua--lua-pattern-quote name))
                      (+lua--current-spec-file))))
