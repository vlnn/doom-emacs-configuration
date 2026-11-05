;;; my/ai/annotate.el -*- lexical-binding: t; -*-
;; SPC e a: explain the region (else line; defun with C-u) one line at a time, beside the code.
;; Explanations stream in as each arrives; an edit, ESC or SPC e a again clears them.
;; SPC e A shows the same explanations as comments over a copy of the code in a popup.

(defvar gptel-model)
(declare-function gptel-request "gptel")

(defconst +ai-annotate-directive
  "You receive source code, each line prefixed by its number and a colon. For every non-blank line output exactly one line of the form `N: explanation`, N being that line's number, saying what the line does in under 80 characters, naming its steps in evaluation order. In stack languages the top of stack is consumed first. Output nothing else: no code, no prose, no fences.")

(defconst +ai-annotate-width 60)

(defface +ai-annotate
  '((t :inherit font-lock-comment-face :slant italic))
  "Explanations beside the code.")

(defvar-local +ai--annotate-run 0
  "Annotations started in this buffer; a reply to an earlier one is dropped.")

(defvar-local +ai--annotate-text ""
  "What has streamed in so far for the current run.")

;;; the reply

;;;###autoload
(defun +ai--annotate-number-lines (text)
  (string-join (seq-map-indexed (lambda (line i) (format "%d: %s" (1+ i) line))
                                (split-string text "\n"))
               "\n"))

;;;###autoload
(defun +ai--annotate-parse (response)
  (let (result)
    (dolist (line (split-string response "\n" t))
      (when (string-match "\\`\\s-*\\([0-9]+\\)[.:)]\\s-*\\(.*\\)\\'" line)
        (push (cons (string-to-number (match-string 1 line))
                    (string-trim (match-string 2 line)))
              result)))
    (nreverse result)))

;;;###autoload
(defun +ai--annotate-explanation (n parsed)
  (string-join (mapcar #'cdr (seq-filter (lambda (e) (= (car e) n)) parsed)) " "))

;;;###autoload
(defun +ai--annotate-complete (text)
  "TEXT up to and including its last newline: the lines that have fully arrived."
  (if-let* ((end (string-match-p "\n[^\n]*\\'" text)))
      (substring text 0 (1+ end))
    ""))

;;; beside the code

;;;###autoload
(defun +ai--annotate-groups (parsed start)
  "((LINE . TEXT) ...) in buffer lines from START, one entry per line, faced."
  (seq-sort-by #'car #'<
               (mapcar (lambda (group)
                         (cons (+ start (car group) -1)
                               (propertize (string-join (mapcar #'cdr (cdr group)) " ") 'face '+ai-annotate)))
                       (seq-group-by #'car parsed))))

;;;###autoload
(defun +ai--annotate-active-p ()
  (seq-some (lambda (o) (overlay-get o '+ai-annotate)) (overlays-in (point-min) (point-max))))

;;;###autoload
(defun +ai--annotate-clear (&rest _)
  "Drop the annotations; t when there were any, so `doom-escape-hook' stops there."
  (remove-hook 'after-change-functions #'+ai--annotate-clear t)
  (prog1 (and (+ai--annotate-active-p) t)
    (setq +ai--annotate-run (1+ +ai--annotate-run)
          +ai--annotate-text "")
    (+ai--overlay-clear '+ai-annotate)))

;;;###autoload
(defun +ai--annotate-place (start parsed)
  "Draw PARSED afresh beside the lines counted from START; the next edit clears them."
  (+ai--overlay-clear '+ai-annotate)
  (let ((column (+ai--overlay-column +ai-flymake-column)))
    (+ai--overlay-render '+ai-annotate +ai-annotate-display (+ai--annotate-groups parsed start)
                         column (+ai--overlay-wrap-width +ai-flymake-wrap-width column)
                         +ai-flymake-margin-width))
  (add-hook 'after-change-functions #'+ai--annotate-clear nil t))

;;;###autoload
(defun +ai--annotate-current-run-p (buffer run)
  (and (buffer-live-p buffer) (= run (buffer-local-value '+ai--annotate-run buffer))))

;;;###autoload
(defun +ai--annotate-stream (buffer start chunk)
  (with-current-buffer buffer
    (setq +ai--annotate-text (concat +ai--annotate-text chunk))
    (+ai--annotate-place start (+ai--annotate-parse (+ai--annotate-complete +ai--annotate-text)))))

;;;###autoload
(defun +ai--annotate-finish (buffer start)
  (with-current-buffer buffer
    (let ((parsed (+ai--annotate-parse +ai--annotate-text)))
      (+ai--annotate-place start parsed)
      (message "Annotated %d lines" (length (seq-uniq (mapcar #'car parsed)))))))

;;;###autoload
(defun +ai--annotate-callback (buffer start run)
  (lambda (response info)
    (when (+ai--annotate-current-run-p buffer run)
      (pcase response
        ((pred stringp) (+ai--annotate-stream buffer start response))
        ('t (+ai--annotate-finish buffer start))
        ('nil (message "Annotate failed: %s" (plist-get info :status)))))))

;;;###autoload
(defun +ai/annotate (&optional whole-defun)
  "Explain the region, else the current line, one line at a time, beside the code.
With WHOLE-DEFUN (\\[universal-argument]) explain the defun at point. Explanations
stream in; an edit, \\[doom/escape] or this command again clears them."
  (interactive "P")
  (require 'gptel)
  (if (+ai--annotate-clear)
      (message "Annotations cleared")
    (pcase-let* ((`(,beg . ,end) (+ai--bounds-at-point whole-defun))
                 (source (buffer-substring-no-properties beg end))
                 (gptel-model +ai-fast-model))
      (gptel-request (+ai--annotate-number-lines source)
        :system +ai-annotate-directive
        :stream t
        :callback (+ai--annotate-callback (current-buffer) (line-number-at-pos beg) +ai--annotate-run)))))

;;; in a popup, as comments over a copy of the code

;;;###autoload
(defun +ai--annotate-wrap (text)
  (with-temp-buffer
    (insert text)
    (let ((fill-column +ai-annotate-width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun +ai--annotate-indentation (line)
  (if (string-match "\\`\\s-*" line) (match-string 0 line) ""))

;;;###autoload
(defun +ai--annotate-comment-lines (n text indent)
  (let* ((prefix (concat indent (string-trim (or comment-start "#")) " "))
         (label (format "%d. " n))
         (pad (make-string (length label) ?\s))
         (lines (+ai--annotate-wrap text)))
    (cons (concat prefix label (car lines))
          (mapcar (lambda (l) (concat prefix pad l)) (cdr lines)))))

;;;###autoload
(defun +ai--annotate-line (line n parsed)
  (let ((explanation (+ai--annotate-explanation n parsed)))
    (if (string-empty-p explanation)
        line
      (string-join (append (+ai--annotate-comment-lines
                            n explanation (+ai--annotate-indentation line))
                           (list line))
                   "\n"))))

;;;###autoload
(defun +ai--annotate-render (source parsed)
  (string-join (seq-map-indexed (lambda (line i) (+ai--annotate-line line (1+ i) parsed))
                                (split-string source "\n"))
               "\n"))

;;;###autoload
(defun +ai--annotate-buffer (mode)
  (with-current-buffer (get-buffer-create "*gptel-annotate*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p mode)
        (delay-mode-hooks (funcall mode)))
      (insert "Explaining...")
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))
    (current-buffer)))

;;;###autoload
(defun +ai--annotate-show (buffer source response)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+ai--annotate-render source (+ai--annotate-parse response))))))

;;;###autoload
(defun +ai--annotate-popup-callback (source buffer)
  (lambda (response info)
    (pcase response
      ((pred stringp) (+ai--annotate-show buffer source response))
      ('nil (message "Annotate failed: %s" (plist-get info :status))))))

;;;###autoload
(defun +ai/annotate-popup (&optional whole-defun)
  "Explain the region, else the current line, as comments over a copy in a popup.
With WHOLE-DEFUN (\\[universal-argument]) explain the defun at point."
  (interactive "P")
  (require 'gptel)
  (let* ((source (+ai--text-at-point whole-defun))
         (buffer (+ai--annotate-buffer major-mode))
         (gptel-model +ai-fast-model))
    (pop-to-buffer buffer)
    (gptel-request (+ai--annotate-number-lines source)
      :system +ai-annotate-directive
      :callback (+ai--annotate-popup-callback source buffer))))
