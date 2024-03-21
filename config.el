;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Iosevka Term")
(setq font-family "M+1Code Nerd Font Mono")
(setq doom-font (font-spec :family font-family :size 15 :weight 'regular)
      doom-symbol-font (font-spec :family font-family :size 15 :weight 'regular)
      doom-big-font (font-spec :family font-family :size 19 :weight 'regular))

;; Using Macbook is hard. But we'll manage
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control
        dired-use-ls-dired nil))

;; Start as big as possible
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; The best clean light theme I've found yet
(use-package! stimmung-themes
  :demand t
  :config
  (stimmung-themes-load-light))

;; I like absolute line numbers. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; I don't like how word-wrap is working
(setq word-wrap nil)

;; I don't like to comment out block of lisp with ;
(defmacro comment (&rest _body)
  "Comment out one or more s-expressions."
  nil)

;; default undo settings are too aggregative to my likings -- undoing these!
;; NB: This is important! Do not delete or modify or another year of hellish ux is coming
(setq evil-want-fine-undo t)

;; how many times did you SPC m e e
;; just to see result of some internal sexp evaluation?
;; Too many, not anymore!
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)
(setq evil-highlight-closing-paren-at-point-states nil)

;; Colors and blinks setup
(blink-cursor-mode 1)
(set-cursor-color "dark green")
(setq-default line-spacing 1)
(use-package! hl-line
  :custom-face
  (hl-line ((t (:background "#d5f7d5")))))
(visual-fill-column-mode -1)
(setq visual-fill-column-split-window-sensibly t)
(mouse-avoidance-mode 'animate)
(setq mouse-avoidance-threshold 0.5)

(setq completion-ignored-extensions
      '(".a"
        ".aux"
        ".bbl"
        ".bin"
        ".blg"
        ".class"
        ".cp"
        ".cps"
        ".elc"
        ".fmt"
        ".fn"
        ".fns"
        ".git/"
        ".glo"
        ".glob"
        ".gmo"
        ".hg/"
        ".idx"
        ".ky"
        ".kys"
        ".la"
        ".lib"
        ".ln"
        ".lo"
        ".lof"
        ".lot"
        ".mem"
        ".mo"
        ".o"
        ".pg"
        ".pgs"
        ".pyc"
        ".pyo"
        ".so"
        ".tfm"
        ".toc"
        ".tp"
        ".tps"
        ".v.d"
        ".vio"
        ".vo" ".vok" ".vos"
        ".vr"
        ".vrs"
        "~"))

;; Movements schema change, unusual for many
;; Move cursor with 'jkl;', not default evil 'hjkl'
;; Change evil-repeat keys...
(setq evil-snipe-override-evil-repeat-keys nil)
;; ...and then use jkl; instead of hjkl. It's stupid how hard it is to re-map ;!
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map "\;" 'evil-forward-char)
  (define-key evil-motion-state-map "k" 'evil-next-line)
  (define-key evil-motion-state-map "l" 'evil-previous-line))

;; use similar layout for window controls
(map! :map evil-window-map
      ;; Navigation using jkl; not hjkl
      "j"         #'evil-window-left
      "k"         #'evil-window-down
      "l"         #'evil-window-up
      ";"         #'evil-window-right
      ;; Swapping windows using jkl; not hjkl
      "C-h"       #'+evil/window-move-left
      "C-k"       #'+evil/window-move-down
      "C-\;"      #'+evil/window-move-right)

;; generalize movements in result of SPC SPC, g D etc.
(map! :map (minibuffer-mode-map
            ivy-minibuffer-map
            vertico-map)
      :g "C-k" #'next-line
      :g "C-l" #'previous-line)

(after! winum
  (setq winum-scope 'visible
        winum-auto-setup-mode-line t))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enable)))

;; General preferences
;; Common mapping
(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace" "%" #'query-replace
      :desc "Undo abbrev"   "U" #'unexpand-abbrev
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|())
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t
        avy-single-candidate-jump nil)

  ;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
  ;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than g-s-/
  ;; every windows should be avy'ed
  (key-chord-define-global "jk" 'avy-goto-char-timer)           ; default and most useful movement
  (key-chord-define-global "ji" 'evil-avy-goto-line)            ; not sure if it's better than the <line number> G or <line number> gg
  (key-chord-define-global "78" 'sp-beginning-of-previous-sexp) ; get to the beginning of the prev sexp
  (key-chord-define-global "89" 'sp-beginning-of-sexp)          ; get to the beginning of the sexp
  (key-chord-define-global "90" 'sp-end-of-sexp)                ; get to the end of the sexp
  (key-chord-define-global "0-" 'sp-end-of-next-sexp)           ; get to the end of next sexp
  (key-chord-define-global "sd" 'basic-save-buffer)             ; too much of shift-;-w-q-<ENT> in my life
  (key-chord-define-global "j;" 'execute-extended-command))     ; same as M-x, to run emacsy command, but specific for the buffer

(load! "avy-functions.el")
(after! avy
  (setq avy-timeout-seconds 0.7)
  :custom
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ; don't use home row for mapping avy functions in avy-dispactch-alist!
  :config
  (avy-setup-default)

  (add-to-list 'avy-dispatch-alist '(?c . avy-action-exchange))

  (setf (alist-get ?x avy-dispatch-alist) 'avy-action-kill-whole-sexp
        (alist-get ?X avy-dispatch-alist) 'avy-action-kill-whole-defun
        (alist-get ?q avy-dispatch-alist) 'avy-action-rename-sexp          ; jump to and replace sexp (e.g. change function no touching args)
        (alist-get ?Q avy-dispatch-alist) 'avy-action-rename-whole-sexp    ; jump to and replace sexp (e.g. change function no touching args)
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-clone-whole-sexp
        (alist-get ?W avy-dispatch-alist) 'avy-action-clone-whole-defun
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport-whole-sexp
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-defun
        (alist-get ?z avy-dispatch-alist) 'avy-action-zap-to-char
        (alist-get ?m avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?C avy-dispatch-alist) 'avy-action-comment-whole-sexp   ; comment out whole sexp avy'ed
        (alist-get ?i avy-dispatch-alist) 'avy-action-lookup-documentation
        (alist-get ?r avy-dispatch-alist) 'avy-action-lookup-references
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char))

(load! "clojure.el")

;; To kill trailing whitespaces... but being sane killer not removing whitespace I just typed in
(use-package! ws-butler
  :config
  (setq ws-butler-keep-whitespace-before-point t))

;; TABs should never be inserted at all
;; (I might need to change it for python or YAML -- but hopefully I don't need them)
(setq-default indent-tabs-mode nil)

;; TAB should work both for line indent and autocompletion
(setq-default c-basic-offset tab-width)
(setq tab-always-indent 'complete)

(load! "magit.el")
(load! "org-mode.el")
(load! "json.el")

(use-package! abbrev
  :config
  (setq abbrev-file-name (concat doom-user-dir "abbrev_defs"))
  (setq save-abbrevs 'silently))
(setq-default abbrev-mode 1)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        evil-forward-char
        evil-backward-char
        evil-previous-line
        evil-delete-backward-char-and-join
        next-line))

(load! "codeium.el")

(use-package! beacon
  :config
  (setq beacon-size 80
        beacon-blink-delay 0.1
        beacon-blink-when-focused 't
        beacon-blink-when-point-moves-vertically 3))

(use-package! lsp
  :config
                                        ; this should make stuff faster... so I've been told.
  (setq font-lock-maximum-decoration 1))

(use-package! org-shortcut)
(load! "secrets.el")

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
  
  (select-frame-by-name "remember")
  (org-capture))

(defun try/switch-to-thing ()
  "Switch to a buffer, open a recent file, jump to a bookmark, or change
   the theme from a unified interface."
  (interactive)
  (let* ((buffers (mapcar #'buffer-name (buffer-list)))
         (recent-files recentf-list)
         (bookmarks (bookmark-all-names))
         (themes (custom-available-themes))
         (all-options (append buffers recent-files bookmarks
                              (mapcar (lambda (theme) (concat "Theme: " (symbol-name theme))) themes)))
         (selection (completing-read "Switch to: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           '(metadata . ((category . file)))
                                         (complete-with-action action all-options str pred)))
                                     nil t nil 'file-name-history)))
    (pcase selection
      ((pred (lambda (sel) (member sel buffers))) (switch-to-buffer selection))
      ((pred (lambda (sel) (member sel bookmarks))) (bookmark-jump selection))
      ((pred (lambda (sel) (string-prefix-p "Theme: " sel)))
       (load-theme (intern (substring selection (length "Theme: "))) t))
      (_ (find-file selection)))))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
