;;; custom.el -*- lexical-binding: t; -*-

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(csv-mode ghub async))
 '(warning-suppress-types '((org-element-cache))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 110 :italic t))))

;; quick jumps to a specific paren in the code
(after! avy
  (defun avy-goto-open-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump "(+")))
  (add-to-list 'avy-orders-alist '(avy-goto-open-parens . avy-order-closest))
  (map! ("M-o" 'avy-goto-open-parens))

  (defun avy-goto-closed-parens ()
    (interactive)
    (let ((avy-command this-command))   ; for look up in avy-orders-alist
      (avy-jump ")")))
  (add-to-list 'avy-orders-alist '(avy-goto-closed-parens . avy-order-closest))
  (map! ("M-p" 'avy-goto-closed-parens)))

(put 'erase-buffer 'disabled nil)


;; This is fast smartparens access and hintsheet in one
;; (defhydra hydra-smartparens (:hint nil)
;;   "
;;  Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
;; ------------------------------------------------------------------------------------------------------------------------
;;  [_a_] beginning  [_n_] down      [_j_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
;;  [_e_] end        [_N_] bw down   [_J_] bw barf    [_j_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
;;  [_f_] forward    [_p_] up        [_;_] slurp      [_J_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_u_] undo
;;  [_b_] backward   [_P_] bw up     [_:_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
;;   ;; Moving
;;   ("a" sp-beginning-of-sexp)
;;   ("e" sp-end-of-sexp)
;;   ("f" sp-forward-sexp)
;;   ("b" sp-backward-sexp)
;;   ("n" sp-down-sexp)
;;   ("N" sp-backward-down-sexp)
;;   ("p" sp-up-sexp)
;;   ("P" sp-backward-up-sexp)

;;   ;; Slurping & barfing
;;   ("j" sp-backward-slurp-sexp)
;;   ("J" sp-backward-barf-sexp)
;;   (";" sp-forward-slurp-sexp)
;;   (":" sp-forward-barf-sexp)

;;   ;; Wrapping
;;   ("R" sp-rewrap-sexp)
;;   ("j" sp-unwrap-sexp)
;;   ("J" sp-backward-unwrap-sexp)
;;   ("(" sp-wrap-round)
;;   ("{" sp-wrap-curly)
;;   ("[" sp-wrap-square)

;;   ;; Sexp juggling
;;   ("S" sp-split-sexp)
;;   ("s" sp-splice-sexp)
;;   ("r" sp-raise-sexp)
;;   ("j" sp-join-sexp)
;;   ("t" sp-transpose-sexp)
;;   ("A" sp-absorb-sexp)
;;   ("E" sp-emit-sexp)
;;   ("o" sp-convolute-sexp)

;;   ;; Destructive editing
;;   ("c" sp-change-inner :exit t)
;;   ("C" sp-change-enclosing :exit t)
;;   ("k" sp-kill-sexp)
;;   ("K" sp-backward-kill-sexp)
;;   ("w" sp-copy-sexp)
;;   ("u" evil-undo)

;;   ("q" nil)
;;   ("g" nil))
