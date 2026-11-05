;;; my/ai/overlay.el -*- lexical-binding: t; -*-
;; Text beside code, shared by the review (flymake.el) and annotations (annotate.el):
;; in a column past the code, in the right margin, or at the end of the line.
;; Groups are (LINE . TEXT), TEXT carrying its face; TAG names whose overlays they are.

;;;###autoload
(defun +ai--overlay-line-bounds (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (cons (point) (line-end-position))))

;;;###autoload
(defun +ai--overlay-longest-line ()
  (let ((longest 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (end-of-line)
        (setq longest (max longest (current-column)))
        (forward-line 1)))
    longest))

;;;###autoload
(defun +ai--overlay-column (fixed)
  "FIXED when set, else two past the longest line or `fill-column'."
  (or fixed (+ 2 (max fill-column (+ai--overlay-longest-line)))))

;;;###autoload
(defun +ai--overlay-wrap-width (fixed column)
  "FIXED when set, else what is left of the window past COLUMN."
  (or fixed (max 16 (- (window-body-width (or (get-buffer-window) (selected-window))) column 2))))

;;;###autoload
(defun +ai--overlay-make (tag beg end)
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay tag t)
    overlay))

;;;###autoload
(defun +ai--overlay-clear (tag)
  (remove-overlays (point-min) (point-max) tag t))

;;;###autoload
(defun +ai--overlay-stretch-to (column)
  (propertize " " 'display `(space :align-to ,column)))

;;;###autoload
(defun +ai--overlay-wrap (text width)
  (with-temp-buffer
    (insert text)
    (let ((fill-column width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun +ai--overlay-fit (text width)
  (truncate-string-to-width text width nil nil "…"))

;;;###autoload
(defun +ai--overlay-free-rows (line annotated total)
  "How many lines below LINE, up to TOTAL, carry no text of their own."
  (let ((next (seq-min (cons (1+ total) (seq-filter (lambda (l) (> l line)) annotated)))))
    (- next line 1)))

;;;###autoload
(defun +ai--overlay-cut (line width)
  (concat (truncate-string-to-width line (1- width)) "…"))

;;;###autoload
(defun +ai--overlay-chunks (text width rows)
  "TEXT wrapped to WIDTH over its own row and ROWS below, cut with an ellipsis past that."
  (let* ((face (get-text-property 0 'face text))
         (lines (+ai--overlay-wrap (substring-no-properties text) width))
         (kept (seq-take lines (1+ rows)))
         (cut (< (length kept) (length lines))))
    (mapcar (lambda (line) (propertize line 'face face))
            (if cut
                (append (butlast kept) (list (+ai--overlay-cut (car (last kept)) width)))
              kept))))

;;;###autoload
(defun +ai--overlay-column-overlays (tag line chunks column)
  (seq-map-indexed (lambda (chunk i)
                     (let ((eol (cdr (+ai--overlay-line-bounds (+ line i)))))
                       (overlay-put (+ai--overlay-make tag eol eol)
                                    'after-string (concat (+ai--overlay-stretch-to column) " " chunk))))
                   chunks))

;;;###autoload
(defun +ai--overlay-render-column (tag groups column width &optional blocked)
  "Draw GROUPS at COLUMN, wrapped to WIDTH; no text wraps onto an annotated line or onto BLOCKED."
  (let ((annotated (delq nil (cons blocked (mapcar #'car groups))))
        (total (count-lines (point-min) (point-max))))
    (pcase-dolist (`(,line . ,text) groups)
      (+ai--overlay-column-overlays
       tag line (+ai--overlay-chunks text width (+ai--overlay-free-rows line annotated total)) column))))

;;;###autoload
(defun +ai--overlay-eol (tag line text)
  (let ((eol (cdr (+ai--overlay-line-bounds line))))
    (overlay-put (+ai--overlay-make tag eol eol) 'after-string (concat "  ⟵ " text))))

;;;###autoload
(defun +ai--overlay-margin (tag line text width)
  (let ((bol (car (+ai--overlay-line-bounds line))))
    (overlay-put (+ai--overlay-make tag bol bol)
                 'before-string (propertize " " 'display `((margin right-margin) ,(+ai--overlay-fit text width))))))

;;;###autoload
(defun +ai--overlay-render (tag display groups column width margin-width &optional blocked)
  "Draw GROUPS as DISPLAY, one of `column', `margin' or `eol'."
  (pcase display
    ('column (+ai--overlay-render-column tag groups column width blocked))
    ('margin (pcase-dolist (`(,line . ,text) groups) (+ai--overlay-margin tag line text margin-width)))
    ('eol (pcase-dolist (`(,line . ,text) groups) (+ai--overlay-eol tag line text)))
    (_ (error "Unknown display %S; expected one of (column margin eol)" display))))
