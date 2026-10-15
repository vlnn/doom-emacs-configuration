;;; my/lua/love.el -*- lexical-binding: t; -*-

(defvar +lua-interrupt-file)

(defconst +lua-love-repl-file
  (expand-file-name "love-repl.lua"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst +lua-love-hook-line
  "if os.getenv(\"EMACS_LOVE_REPL\") then dofile(os.getenv(\"EMACS_LOVE_REPL\")) end"
  "Line for a LÖVE game's conf.lua that lets Emacs run the REPL inside the game.")

(defvar +lua-love-command
  (list (or (executable-find "love") "/Applications/love.app/Contents/MacOS/love"))
  "Program and leading arguments that run a LÖVE game; the game root is appended.")

(defun +lua--file-mentions-p (file text)
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (search-forward text nil t))
       t))

(defun +lua--love-hooked-p (root)
  (seq-some (lambda (name) (+lua--file-mentions-p (expand-file-name name root) "EMACS_LOVE_REPL"))
            '("conf.lua" "main.lua")))

(defun +lua--with-love-hook (conf)
  (cond ((string-search +lua-love-hook-line conf) conf)
        ((string-empty-p conf) (concat +lua-love-hook-line "\n"))
        (t (concat +lua-love-hook-line "\n\n" conf))))

(defun +lua/add-love-hook ()
  "Put the line that runs the Emacs REPL inside the game at the top of conf.lua."
  (interactive)
  (let ((conf (expand-file-name "conf.lua" (+lua--project-root))))
    (with-current-buffer (find-file-noselect conf)
      (let ((hooked (+lua--with-love-hook (buffer-string))))
        (unless (equal hooked (buffer-string))
          (erase-buffer)
          (insert hooked)
          (save-buffer)))
      (message "conf.lua runs the Emacs REPL when the game starts from Emacs"))))

(defun +lua--love-environment (interrupt-file)
  (list (concat "EMACS_LOVE_REPL=" +lua-love-repl-file)
        (concat "EMACS_LOVE_REPL_INTERRUPT=" interrupt-file)))

(defun +lua--prompt-shown-p ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^> " nil t)))

(defun +lua--await-first-prompt (proc)
  (with-current-buffer (process-buffer proc)
    (with-timeout (30 (user-error "LÖVE started no REPL; is %s in conf.lua?" +lua-love-hook-line))
      (while (and (process-live-p proc) (not (+lua--prompt-shown-p)))
        (accept-process-output proc 0.1))))
  (unless (process-live-p proc)
    (user-error "LÖVE exited before its REPL started; see the *lua* buffer")))

(defun +lua--launch-love (root)
  (let ((default-directory root)
        (process-connection-type nil)
        (process-environment (append (+lua--love-environment +lua-interrupt-file)
                                     process-environment)))
    (get-buffer-process
     (apply #'make-comint-in-buffer "lua" "*lua*" (car +lua-love-command) nil
            (append (cdr +lua-love-command) (list root))))))

(defun +lua--start-love-repl (root)
  (let ((proc (+lua--launch-love root)))
    (process-put proc '+lua-interrupt-file +lua-interrupt-file)
    (process-put proc '+lua-evaluator t)
    (+lua--adopt-repl proc)
    (+lua--await-first-prompt proc)
    proc))
