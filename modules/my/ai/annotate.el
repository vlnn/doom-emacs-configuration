;;; my/ai/annotate.el -*- lexical-binding: t; -*-
;; SPC o l E: show the region (else line; defun with C-u) with a numbered explanation per line.

(defconst +ai-annotate-directive
  "You receive source code, each line prefixed by its number and a colon. For every non-blank line output exactly one line of the form `N: explanation`, N being that line's number, saying what the line does in under 80 characters, naming its steps in evaluation order. In stack languages the top of stack is consumed first. Output nothing else: no code, no prose, no fences.")

(defconst +ai-annotate-width 60)

(defun +ai--annotate-number-lines (text)
  (string-join (seq-map-indexed (lambda (line i) (format "%d: %s" (1+ i) line))
                                (split-string text "\n"))
               "\n"))

(defun +ai--annotate-parse (response)
  (let (result)
    (dolist (line (split-string response "\n" t))
      (when (string-match "\\`\\s-*\\([0-9]+\\)[.:)]\\s-*\\(.*\\)\\'" line)
        (push (cons (string-to-number (match-string 1 line))
                    (string-trim (match-string 2 line)))
              result)))
    (nreverse result)))

(defun +ai--annotate-explanation (n parsed)
  (string-join (mapcar #'cdr (seq-filter (lambda (e) (= (car e) n)) parsed)) " "))

(defun +ai--annotate-wrap (text)
  (with-temp-buffer
    (insert text)
    (let ((fill-column +ai-annotate-width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

(defun +ai--annotate-indentation (line)
  (if (string-match "\\`\\s-*" line) (match-string 0 line) ""))

(defun +ai--annotate-comment-lines (n text indent)
  (let* ((prefix (concat indent (string-trim (or comment-start "#")) " "))
         (label (format "%d. " n))
         (pad (make-string (length label) ?\s))
         (lines (+ai--annotate-wrap text)))
    (cons (concat prefix label (car lines))
          (mapcar (lambda (l) (concat prefix pad l)) (cdr lines)))))

(defun +ai--annotate-line (line n parsed)
  (let ((explanation (+ai--annotate-explanation n parsed)))
    (if (string-empty-p explanation)
        line
      (string-join (append (+ai--annotate-comment-lines
                            n explanation (+ai--annotate-indentation line))
                           (list line))
                   "\n"))))

(defun +ai--annotate-render (source parsed)
  (string-join (seq-map-indexed (lambda (line i) (+ai--annotate-line line (1+ i) parsed))
                                (split-string source "\n"))
               "\n"))

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

(defun +ai--annotate-show (buffer source response)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+ai--annotate-render source (+ai--annotate-parse response))))))

(defun +ai--annotate-callback (source buffer)
  (lambda (response info)
    (pcase response
      ((pred stringp) (+ai--annotate-show buffer source response))
      ('nil (message "Annotate failed: %s" (plist-get info :status))))))

(defun +ai/annotate (&optional whole-defun)
  "Explain the region, else the current line, one line at a time.
With WHOLE-DEFUN (\[universal-argument]) explain the defun at point."
  (interactive "P")
  (let* ((source (+ai--text-at-point whole-defun))
         (buffer (+ai--annotate-buffer major-mode))
         (gptel-model +ai-fast-model))
    (pop-to-buffer buffer)
    (gptel-request (+ai--annotate-number-lines source)
      :system +ai-annotate-directive
      :callback (+ai--annotate-callback source buffer))))

(set-popup-rule! "^\\*gptel-annotate\\*$"
  :side 'right :size 0.5 :select t :quit t :ttl nil)

(map! :leader :desc "Explain annotated" "o l E" #'+ai/annotate)
