;;; my/lua/repl.el -*- lexical-binding: t; -*-

(require 'comint)
(require 'subr-x)

(defvar lua-process)
(defvar lua-process-buffer)

(defconst +lua-evaluator-file
  (expand-file-name "evaluator.lua"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defconst +lua--reply-regexp "\2\\([a-z]+\\)\37\\(\\(?:.\\|\n\\)*?\\)\3")

(defconst +lua--reply-end "\3\n")

(defconst +lua--reply-end-regexp
  (rx (or "\3" (seq bol (? (+ (not (any " \n"))) ": ") "stdin:" (+ digit) ":" (* anything)))
      "\n")
  "End of an evaluator reply, or of an error the REPL itself printed.")

(defvar +lua--last-source-buffer nil)

;;; Protocol

(defun +lua--escape-char (char)
  (pcase char
    ("\n" "\\n")
    ("\r" "\\r")
    ("\t" "\\t")
    ((rx cntrl) (format "\\%03d" (string-to-char char)))
    (_ (concat "\\" char))))

(defun +lua--string-literal (string)
  (concat "'" (replace-regexp-in-string "[\\\\'\"[:cntrl:]]" #'+lua--escape-char string t t) "'"))

(defun +lua--eval-command (code-file chunk-name line)
  (format "__emacs_eval_file(%s, %s, %d)"
          (+lua--string-literal code-file) (+lua--string-literal chunk-name) line))

(defun +lua--load-evaluator-command (path)
  (format "dofile(%s) io.write(%s)"
          (+lua--string-literal path) (+lua--string-literal +lua--reply-end)))

(defun +lua--parse-reply (output)
  (when (string-match +lua--reply-regexp output)
    (list :stdout (substring output 0 (match-beginning 0))
          :status (intern (match-string 1 output))
          :value (match-string 2 output))))

(defun +lua--without-prompt-line (output)
  (replace-regexp-in-string "[^\n]*\\'" "" output))

(defun +lua--unframed-reply (output)
  (list :stdout "" :status 'err :value (string-trim (+lua--without-prompt-line output))))

(defun +lua--reply-text (reply)
  (if (eq (plist-get reply :status) 'err)
      (concat "error: " (plist-get reply :value))
    (plist-get reply :value)))

(defun +lua--as-comment (value)
  (concat "-- => " (string-join (split-string value "\n") "\n-- ")))

;;; Transport

(defun +lua--await-redirect (proc)
  (while (and (not comint-redirect-completed) (process-live-p proc))
    (accept-process-output proc 0.05)))

(defun +lua--abandon-redirect (proc)
  (interrupt-process proc)
  (with-timeout (1 nil)
    (+lua--await-redirect proc))
  (unless comint-redirect-completed
    (comint-redirect-cleanup)
    (process-put proc '+lua-busy t)))

(defun +lua--late-reply-arrived-p (proc)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (and (looking-at-p comint-prompt-regexp)
           (looking-back +lua--reply-end (- (point) (length +lua--reply-end)))))))

(defun +lua--busy-p (proc)
  (when (and (process-get proc '+lua-busy) (+lua--late-reply-arrived-p proc))
    (process-put proc '+lua-busy nil))
  (process-get proc '+lua-busy))

(defun +lua--send-redirected (proc command output)
  "Redirect until a reply terminator and prompt arrive; bare prompts don't count."
  (let ((comint-prompt-regexp (concat +lua--reply-end-regexp comint-prompt-regexp)))
    (comint-redirect-send-command-to-process command output proc nil t)))

(defun +lua--redirect (proc command)
  "Send COMMAND to PROC and return its output instead of showing it in the REPL."
  (let ((output (generate-new-buffer " *lua-eval*" t))
        (comint-redirect-perform-sanity-check nil)
        (comint-redirect-insert-matching-regexp t))
    (unwind-protect
        (with-current-buffer (process-buffer proc)
          (unwind-protect
              (progn
                (+lua--send-redirected proc command output)
                (+lua--await-redirect proc))
            (unless comint-redirect-completed
              (+lua--abandon-redirect proc)))
          (with-current-buffer output (buffer-string)))
      (kill-buffer output))))

(defun +lua--ensure-evaluator (proc)
  (unless (process-get proc '+lua-evaluator)
    (+lua--redirect proc (+lua--load-evaluator-command +lua-evaluator-file))
    (process-put proc '+lua-evaluator t)))

(defun +lua--busy-reply ()
  (list :stdout "" :status 'err
        :value "Lua REPL is busy with an interrupted eval; restart it with +lua/restart-repl"))

(defun +lua--write-code-file (code)
  (let ((coding-system-for-write 'utf-8-unix))
    (make-temp-file "emacs-lua-eval" nil ".lua" code)))

(defun +lua--reply-from (proc output)
  (or (+lua--parse-reply output)
      (progn
        (process-put proc '+lua-evaluator nil)
        (+lua--unframed-reply output))))

(defun +lua--eval-in-process (proc code chunk-name line)
  (if (+lua--busy-p proc)
      (+lua--busy-reply)
    (+lua--ensure-evaluator proc)
    (let ((code-file (+lua--write-code-file code)))
      (unwind-protect
          (+lua--reply-from proc (+lua--redirect proc (+lua--eval-command code-file chunk-name line)))
        (delete-file code-file)))))

(defun +lua--insert-before-prompt (proc text)
  (with-current-buffer (process-buffer proc)
    (save-excursion
      (goto-char (process-mark proc))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (insert text)))))

(defun +lua--echo-to-repl (proc reply)
  (let ((stdout (plist-get reply :stdout)))
    (unless (string-empty-p stdout)
      (+lua--insert-before-prompt proc stdout))))

;;; REPL lifecycle

(define-minor-mode +lua-repl-mode
  "Cider-style localleader bindings for the Lua REPL buffer."
  :keymap (make-sparse-keymap))

(defun +lua--project-root ()
  (or (doom-project-root) default-directory))

(defun +lua--enable-repl-mode (&rest _)
  (with-current-buffer lua-process-buffer
    (+lua-repl-mode +1)))

(defun +lua--start-repl ()
  (let ((default-directory (+lua--project-root)))
    (lua-start-process))
  lua-process)

(defun +lua--live-process ()
  (require 'lua-mode)
  (if (process-live-p lua-process)
      lua-process
    (+lua--start-repl)))

(defun +lua--remember-source-buffer ()
  (unless (bound-and-true-p +lua-repl-mode)
    (setq +lua--last-source-buffer (current-buffer))))

(defun +lua/open-project-repl (&optional _arg)
  "Return the Lua REPL buffer, starting one at the project root when needed."
  (interactive)
  (+lua--remember-source-buffer)
  (process-buffer (+lua--live-process)))

(defun +lua/switch-to-repl ()
  "Show the Lua REPL, starting one at the project root when needed."
  (interactive)
  (pop-to-buffer (+lua/open-project-repl)))

(defun +lua/switch-to-last-source ()
  "Go back to the Lua buffer the REPL was last opened from."
  (interactive)
  (if (buffer-live-p +lua--last-source-buffer)
      (pop-to-buffer +lua--last-source-buffer)
    (user-error "No Lua source buffer to return to")))

(defun +lua/quit-repl ()
  "Kill the Lua REPL."
  (interactive)
  (lua-kill-process))

(defun +lua/restart-repl ()
  "Kill the Lua REPL and start a fresh one at the project root."
  (interactive)
  (lua-kill-process)
  (+lua/switch-to-repl))

(defun +lua/clear-repl ()
  "Clear the Lua REPL output."
  (interactive)
  (with-current-buffer (+lua/open-project-repl)
    (comint-clear-buffer)))

;;; Evaluation

(defun +lua--chunk-name ()
  (or (buffer-file-name) (buffer-name)))

(defun +lua--eval (beg end)
  (let* ((proc (+lua--live-process))
         (reply (+lua--eval-in-process proc
                                       (buffer-substring-no-properties beg end)
                                       (+lua--chunk-name)
                                       (line-number-at-pos beg))))
    (+lua--echo-to-repl proc reply)
    reply))

(defun +lua--line-or-region-bounds ()
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (cons (line-beginning-position) (line-end-position))))

(defun +lua--defun-bounds ()
  (or (bounds-of-thing-at-point 'defun)
      (user-error "Not inside a function")))

(defun +lua--after-shebang (pos)
  (save-excursion
    (goto-char pos)
    (if (and (bobp) (looking-at-p "#!"))
        (line-beginning-position 2)
      pos)))

(defun +lua/eval-region (beg end)
  "Evaluate the region in the Lua REPL and show the result inline."
  (interactive "r")
  (let ((text (+lua--reply-text (+lua--eval beg end))))
    (save-excursion
      (goto-char end)
      (skip-chars-backward " \t\n" beg)
      (+eval-display-results text (current-buffer)))))

(defun +lua/eval-line-or-region ()
  "Evaluate the active region or current line and show the result inline."
  (interactive)
  (pcase-let ((`(,beg . ,end) (+lua--line-or-region-bounds)))
    (+lua/eval-region beg end)))

(defun +lua/eval-defun ()
  "Evaluate the function around point and show the result inline."
  (interactive)
  (pcase-let ((`(,beg . ,end) (+lua--defun-bounds)))
    (+lua/eval-region beg end)))

(defun +lua/load-buffer ()
  "Evaluate the whole buffer in the Lua REPL."
  (interactive)
  (let ((reply (+lua--eval (+lua--after-shebang (point-min)) (point-max))))
    (if (eq (plist-get reply :status) 'err)
        (+eval-display-results (+lua--reply-text reply) (current-buffer))
      (message "Loaded %s" (buffer-name)))))

(defun +lua/load-buffer-and-switch-to-repl ()
  "Evaluate the whole buffer, then show the Lua REPL."
  (interactive)
  (+lua/load-buffer)
  (+lua/switch-to-repl))

(defun +lua/eval-line-or-region-to-comment ()
  "Evaluate the region or current line, inserting the result as a comment."
  (interactive)
  (pcase-let* ((`(,beg . ,end) (+lua--line-or-region-bounds))
               (text (+lua--reply-text (+lua--eval beg end))))
    (save-excursion
      (goto-char end)
      (end-of-line)
      (insert "\n" (+lua--as-comment text)))))

(defun +lua/eval-line-or-region-to-popup ()
  "Evaluate the active region or current line and show the result in a popup."
  (interactive)
  (pcase-let ((`(,beg . ,end) (+lua--line-or-region-bounds)))
    (+eval-display-results-in-popup (+lua--reply-text (+lua--eval beg end)))))

;;; Sending code to the REPL prompt

(defun +lua--insert-in-repl (beg end)
  (let ((code (buffer-substring-no-properties beg end)))
    (pop-to-buffer (+lua/open-project-repl))
    (goto-char (point-max))
    (insert code)))

(defun +lua/insert-line-or-region-in-repl ()
  "Copy the active region or current line to the REPL prompt."
  (interactive)
  (pcase-let ((`(,beg . ,end) (+lua--line-or-region-bounds)))
    (+lua--insert-in-repl beg end)))

(defun +lua/insert-defun-in-repl ()
  "Copy the function around point to the REPL prompt."
  (interactive)
  (pcase-let ((`(,beg . ,end) (+lua--defun-bounds)))
    (+lua--insert-in-repl beg end)))
