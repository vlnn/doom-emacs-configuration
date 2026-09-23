;;; apl.el -*- lexical-binding: t; -*-

;; gnu-apl-mode + ride-apl.el for Dyalog APL, wired the same way the
;; (clojure +lsp) module wires cider: major mode with eldoc/imenu,
;; APL-Z glyph input, repl/eval handlers, popup rules, localleader.

(use-package! gnu-apl-mode
  :mode ("\\.dyalog\\'" . gnu-apl-mode)
  :mode ("\\.apl[fcnoi]\\'" . gnu-apl-mode)
  :init
  (defun +apl--enable-input-method ()
    (require 'gnu-apl-input)
    (activate-input-method "APL-Z"))
  (add-hook 'gnu-apl-mode-hook #'+apl--enable-input-method)
  (add-hook 'ride-apl-repl-mode-hook #'+apl--enable-input-method)
  (add-hook 'gnu-apl-mode-hook
          (lambda () (setq mode-name "Dyalog")))
  :config
  ;; super is Cmd on macOS; glyphs come from APL-Z, not s- chords
  (setopt gnu-apl-mode-map-prefix "H-")
  ;; these two start a GNU APL process; ride-apl owns eval
  (map! :map gnu-apl-mode-map
        "C-c C-s" nil
        "C-c C-f" nil)
  (map! :localleader
        :map gnu-apl-mode-map
        (:prefix ("h" . "help")
         "a" #'gnu-apl-apropos-symbol
         "d" #'gnu-apl-show-help-for-symbol
         "k" #'gnu-apl-show-keyboard)))

;; Dyalog-style backtick prefix (`i inserts iota); on gnu-apl-input, not the
;; mode, so the REPL gets it even when no APL file has been opened yet.
;; GNU APL's ◊ is not Dyalog's ⋄ statement separator, so fix `` first.
(after! gnu-apl-input
  (setf (alist-get "diamond" gnu-apl--symbols nil nil #'equal)
        '("⋄" "`"))
  (setopt gnu-apl-key-prefix ?\`))

(use-package! ride-apl
  :commands (ride-apl-connect ride-apl-eval-minor-mode)
  :init
  (add-hook 'gnu-apl-mode-hook #'ride-apl-eval-minor-mode)
  :config
  (set-repl-handler! 'gnu-apl-mode #'+apl/open-repl)
  (set-eval-handler! 'gnu-apl-mode #'ride-apl-eval-region)

  ;; ride-apl prefers dyalog-mode here, which is no longer installed
  (defadvice! +apl--edit-buffers-use-gnu-apl (&rest _)
    :override #'ride-apl-edit--ensure-major-mode
    (unless (eq major-mode 'gnu-apl-mode)
      (gnu-apl-mode)))

  (set-popup-rules!
    '(("^\\*ride-apl-repl:" :quit nil :ttl nil)
      ("^\\*ride-apl-log:"  :ignore t)))

  (map! :localleader
        :map gnu-apl-mode-map
        "'" #'ride-apl-connect
        "c" #'ride-apl-connect
        (:prefix ("d" . "debug")
         "d" #'ride-apl-trace)
        (:prefix ("e" . "eval")
         "b" #'ride-apl-eval-buffer
         "e" #'ride-apl-eval-line-or-region
         "r" #'ride-apl-eval-region)
        (:prefix ("g" . "goto")
         "g" #'ride-apl-edit-at-point
         "G" #'ride-apl-edit)
        (:prefix ("r" . "repl")
         "b" #'ride-apl-pop-to-repl
         "l" #'ride-apl-load-file
         "q" #'ride-apl-disconnect
         "s" #'ride-apl-transcript-save
         "w" #'ride-apl-set-width))

  (map! :localleader
        :map ride-apl-repl-mode-map
        "e" #'ride-apl-edit-at-point
        "q" #'ride-apl-disconnect
        "s" #'ride-apl-transcript-save
        "w" #'ride-apl-set-width))

(defun +apl/open-repl (&optional _arg)
  "Return the RIDE REPL buffer, connecting first when there is no session."
  (interactive)
  (require 'ride-apl)
  (let ((conn (or (ride-apl-current-conn)
                  (call-interactively #'ride-apl-connect))))
    (ride-apl-conn-repl-buffer conn)))

(defvar +apl-font-families '("APL387" "APL385 Unicode"))
(defvar +apl-font-height 1.1)

(defun +apl--installed-font-family ()
  (seq-find (lambda (family) (find-font (font-spec :family family)))
            +apl-font-families))

(defun +apl--use-apl-font ()
  (when-let ((family (+apl--installed-font-family)))
    (buffer-face-set `(:family ,family :height ,+apl-font-height))))


(add-hook 'gnu-apl-mode-hook #'+apl--use-apl-font)
(add-hook 'ride-repl-mode-hook #'+apl--use-apl-font)
