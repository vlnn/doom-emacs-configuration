;;; my/ai/jail.el -*- lexical-binding: t; -*-
;; Run pi, hax and task tests under macOS Seatbelt with agent.sb.

(defvar +ai-jail-enabled t
  "Jail agents when the OS can; nil runs them with your full rights.")

(defconst +ai-jail-dir
  (file-name-directory (or load-file-name (bound-and-true-p byte-compile-current-file) buffer-file-name)))

(defvar +ai-jail-profile (expand-file-name "agent.sb" +ai-jail-dir)
  "Seatbelt profile for agents, read from the live config so a project cannot swap it.")

(defconst +ai-jail-networks '("open" "local")
  "Network modes agent.sb knows: everything, or loopback only.")

(defvar +ai--jail-warned nil)

(defun +ai--jail-available-p ()
  (and (eq system-type 'darwin) (executable-find "sandbox-exec") t))

(defun +ai--real-dir (dir)
  (directory-file-name (file-truename dir)))

(defun +ai--jail-param (name value)
  (list "-D" (concat name "=" value)))

(defun +ai--jail-params (root network)
  (append (+ai--jail-param "PROJECT" (+ai--real-dir root))
          (+ai--jail-param "HOME" (+ai--real-dir "~"))
          (+ai--jail-param "TMPDIR" (+ai--real-dir temporary-file-directory))
          (+ai--jail-param "NETWORK" network)))

(defun +ai--warn-unjailed ()
  (unless +ai--jail-warned
    (setq +ai--jail-warned t)
    (message "No sandbox-exec here: agents run with your full rights")))

(defun +ai--jailed (root command &optional network)
  "COMMAND wrapped in sandbox-exec for an agent working in ROOT, when jailing applies.
NETWORK is \"open\" (the default) or \"local\" for loopback only."
  (let ((network (or network "open")))
    (unless (member network +ai-jail-networks)
      (error "Unknown jail network %S; expected one of %S" network +ai-jail-networks))
    (cond ((or (not +ai-jail-enabled) (file-remote-p root)) command)
          ((not (+ai--jail-available-p)) (+ai--warn-unjailed) command)
          (t (append (list "sandbox-exec" "-f" +ai-jail-profile) (+ai--jail-params root network) command)))))
