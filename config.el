;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; TODO each {{{ block }}} below should be perhaps moved to it's own file,
;; but I like to think about it more than to do it

;; Common editor configuration shared between all the modes
;; Fantasque is great font, so let's use it where possible for text.
(setq font-family "Iosevka Term")
(setq doom-font (font-spec :family font-family :size 13)
      doom-symbol-font (font-spec :family font-family :size 13)
      doom-big-font (font-spec :family font-family :size 19))

;; Using Macbook is hard. But we'll manage
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'control))

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
(use-package! hl-line
  :custom-face
  (hl-line ((t (:background "#d5f7d5")))))
(mouse-avoidance-mode 'nil)

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

(after! winum
  (setq winum-scope 'visible
        winum-auto-setup-mode-line t))

;; General preferences
;; Common mapping
(map! :leader
      :desc "Query replace regexp" "#" #'query-replace-regexp
      :desc "Query replace" "%" #'query-replace
      :desc "Undo abbrev"   "U" #'unexpand-abbrev
      :desc "Consult flycheck" "F" #'consult-flycheck)

;; smartparens is BAD if you have parinfer (e.g. it autocompletes (|)() instead of (|())
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; This is *so good* I recommend you use it for every "unusual" movement around the visible part of emacs
;; To use avy-goto-char-timer as main driver it should be mapped to something shorter than SPACE-g-s-/
;; every windows should be avy'ed
(key-chord-mode 1)
(after! key-chord
  (setq avy-all-windows t)
  (key-chord-define-global "jk" 'avy-goto-char-timer))

;; Clojure (and cider)
(after! cider
  (add-to-list 'exec-path "/home/va/.asdf/shims")
  (setq nrepl-use-ssh-fallback-for-remote-hosts t) ;; Cider should be able to connect to remote hosts using ssh
  (setq cider-eldoc-display-context-dependent-info t)
  ;(setq clojure-align-forms-automatically t)
  (map! :after clojure-mode :map clojure-mode-map :localleader ;; faster simpler workflow shortcuts
        ("f" #'consult-flycheck)
        ("m" #'cider-selector)
        ("P" nil)
        ("p" nil)
        ("\;" #'cider-pprint-eval-last-sexp-to-comment)
        ("N" #'cider-test-run-ns-tests)
        ("T" #'projectile-toggle-between-implementation-and-test))
  (setq cider-print-options '(("length" 20) ("right-margin" 70)))
  (evil-make-intercept-map cider--debug-mode-map 'normal) ;; don't mess evil-mode with cider debug
  (add-hook 'cider-inspector-mode-hook #'evil-normalize-keymaps))

(use-package! parinfer-rust-mode
  :init
  (setq parinfer-rust-library "~/.config/emacs/.local/etc/parinfer-rust/libparinfer_rust.dylib") ; due to MacOS on M1. Had to compile it and put in this folder.
  :config
  (map! :map parinfer-rust-mode-map
        :localleader
        "p" #'cider-pprint-eval-last-sexp
        "[" #'parinfer-rust-switch-mode
        "P" #'cider-pprint-eval-last-sexp-to-comment
        "{" #'parinfer-rust-toggle-disable))

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

(after! magit
  ;; magit difftastic setup

  (defun th/magit--with-difftastic (buffer command)
    "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
    (let ((process-environment
           (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                         (number-to-string (frame-width)))
                 process-environment)))
      ;; Clear the result buffer (we might regenerate a diff, e.g., for
      ;; the current changes in our working directory).
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer))
      ;; Now spawn a process calling the git COMMAND.
      (make-process
       :name (buffer-name buffer)
       :buffer buffer
       :command command
       ;; Don't query for running processes when emacs is quit.
       :noquery t
       ;; Show the result buffer once the process has finished.
       :sentinel (lambda (proc _)
                   (when (eq (process-status proc) 'exit)
                     (with-current-buffer (process-buffer proc)
                       (goto-char (point-min))
                       (ansi-color-apply-on-region (point-min) (point-max))
                       (setq buffer-read-only t)
                       (view-mode)
                       (end-of-line)
                       ;; difftastic diffs are usually 2-column side-by-side,
                       ;; so ensure our window is wide enough.
                       (let ((width (current-column)))
                         (while (zerop (forward-line 1))
                           (end-of-line)
                           (setq width (max (current-column) width)))
                         ;; Add column size of fringes
                         (setq width (+ width
                                        (fringe-columns 'left)
                                        (fringe-columns 'right)))
                         (goto-char (point-min))
                         (pop-to-buffer
                          (current-buffer)
                          `(;; If the buffer is that wide that splitting the frame in
                            ;; two side-by-side windows would result in less than
                            ;; 80 columns left, ensure it's shown at the bottom.
                            ,(when (> 80 (- (frame-width) width))
                               #'display-buffer-at-bottom)
                            (window-width
                             . ,(min width (frame-width))))))))))))


  (defun th/magit-show-with-difftastic (rev)
    "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If REV is given, just use it.
            ;(when (boundp 'rev) rev)
            ;; If not invoked with prefix arg, try to guess the REV from
            ;; point's position.
            (and (not current-prefix-arg)
                 (or (magit-thing-at-point 'git-revision t)
                     (magit-branch-or-commit-at-point)))
            ;; Otherwise, query the user.
            (magit-read-branch-or-commit "Revision"))))
    (if (not rev)
        (error "No revision specified")
      (th/magit--with-difftastic
       (get-buffer-create (concat "*git show difftastic " rev "*"))
       (list "git" "--no-pager" "show" "--ext-diff" rev))))


  (defun th/magit-diff-with-difftastic (arg)
    "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
    (interactive
     (list (or
            ;; If RANGE is given, just use it.
            ;(when (boundp 'range) range)
            ;; If prefix arg is given, query the user.
            (and current-prefix-arg
                 (magit-diff-read-range-or-commit "Range"))
            ;; Otherwise, auto-guess based on position of point, e.g., based on
            ;; if we are in the Staged or Unstaged section.
            (pcase (magit-diff--dwim)
              ('unmerged (error "unmerged is not yet implemented"))
              ('unstaged nil)
              ('staged "--cached")
              (`(commit . ,value) (format "%s^..%s" value value))
              ((and range (pred stringp)) range)
              (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
    (let ((name (concat "*git diff difftastic"
                        (if arg (concat " " arg) "")
                        "*")))
      (th/magit--with-difftastic
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)]

    (transient-append-suffix 'magit-dispatch "!"
      '("#" "My Magit Cmds" th/magit-aux-commands))

    (define-key magit-status-mode-map (kbd "#") #'th/magit-aux-commands)))


(after! org
  :config
  (setq org-directory "~/Documents/org"
        org-default-notes-file "~/Documents/org/notes.org"
        org-agenda-files (list "~/Documents/org/"))
  (setq org-tags-column -80
        org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
                            (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))
       cfw:org-overwrite-default-keybinding t)

 (setq org-capture-templates
       '(("a" "Work todo" entry
          (file+headline "paid-tasks.org" "Work tasks")
          "* TODO @paid %?\n%i\n%a" :prepend t)
         ("t" "Personal todo" entry
          (file+headline "todo.org" "Personal")
          "* TODO %?\n%i\n%a" :prepend t)
         ("n" "Personal notes" entry
          (file+headline "inbox.org" "Inbox")
          "* %u %?\n%i\n%a" :prepend t)
         ("j" "Journal" entry
          (file+olp+datetree "journal.org")
          "* %U %?\n%i\n%a" :prepend t)
         ("p" "Templates for projects")
         ("pt" "Project-local todo" entry
          (file+headline "projects.org" "Inbox")
          "* TODO %?\n%i\n%a" :prepend t)
         ("pn" "Project-local notes" entry
          (file+headline "notes.org" "Inbox")
          "* %U %?\n%i\n%a" :prepend t)))

 (setq org-agenda-span 5
       org-agenda-start-day "-2d"
       org-agenda-start-with-clockreport-mode t
       org-agenda-clockreport-parameter-plist '(:stepskip0 t :link t :maxlevel 2 :fileskip0 t) ; no empty records in the agenda
       org-agenda-start-with-log-mode t
       org-agenda-start-with-follow-mode t
       org-agenda-include-diary t
       org-habit-show-all-today t
       org-habit-show-done-always-green t
       org-habit-preceding-days 7
       org-habit-following-days 2
       org-use-property-inheritance t))


;;; In ~/.doom.d/config.el
;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))
;; To disable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))

(setq-default abbrev-mode 1)
(setq abbrev-file-name (concat doom-user-dir "abbrev_defs"))
(setq save-abbrevs 'silently)

(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-excluded-commands
      '(self-insert-command
        evil-forward-char
        evil-backward-char
        evil-previous-line
        evil-delete-backward-char-and-join
        next-line))

(defun my/cape-codeium (&optional interactive)
  "Allow codeium capf to be run by itself"
  (interactive (list t))
  (when interactive
    ;; if also testing copilot, clear their overlay before showing capf popup
    (when (bound-and-true-p copilot-mode) (copilot-clear-overlay))
    (cape-interactive #'codeium-completion-at-point)))
(map! :leader
      :desc "Try AI" "A" #'my/cape-codeium)
(keymap-global-set "C-c a i" #'my/cape-codeium)

(use-package! codeium
  :config
  (add-hook! prog-mode (add-hook 'completion-at-point-functions #'codeium-completion-at-point 100 t)))

(use-package! why-this
  :hook (prog-mode . why-this-mode)
  :config
  (setq why-this-annotate-author-length 10
        why-this-annotate-width 50)
  (set-face-background 'why-this-annotate-heat-map-cold "#0de3f4")
  (set-face-background 'why-this-face "#f3fff4")
  (set-face-foreground 'why-this-face "#7d8d9d"))

(use-package! annotate
  :hook (prog-mode . annotate-mode)
  :defer t
  :config
  (setq annotate-database-confirm-deletion t
        annotate-annotation-position-policy :margin))

(use-package! beacon
  :config
  (setq beacon-size 80
        beacon-blink-delay 0.1
        beacon-blink-when-focused 't
        beacon-blink-when-point-moves-vertically 3))
