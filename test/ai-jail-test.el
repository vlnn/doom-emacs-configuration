;;; test/ai-jail-test.el -*- lexical-binding: t; -*-

(defvar pilish-executable)
(defvar pilish-extra-args)
(defvar pilish-project-trust-policy)

(defmacro +ai-jail-test--available (&rest body)
  "Run BODY as if sandbox-exec were installed and jailing on."
  `(let ((+ai-jail-enabled t)
         (+ai-jail-profile "/p/agent.sb"))
     (cl-letf (((symbol-function '+ai--jail-available-p) (lambda () t)))
       ,@body)))

(defun +ai-jail-test--param (name command)
  (seq-find (lambda (arg) (string-prefix-p (concat name "=") arg)) command))

;;; command wrapping

(ert-deftest +ai--jailed/wraps-command-in-sandbox-exec ()
  (+ai-jail-test--available
   (let ((command (+ai--jailed temporary-file-directory '("pi" "--mode" "rpc"))))
     (ert-info ("jailed should start sandbox-exec with the agent profile")
       (should (equal '("sandbox-exec" "-f" "/p/agent.sb") (seq-take command 3))))
     (ert-info ("jailed should keep the original command, arguments and all, last")
       (should (equal '("pi" "--mode" "rpc") (last command 3)))))))

(ert-deftest +ai--jailed/passes-real-paths-as-profile-params ()
  (+ai-jail-test--available
   (let ((command (+ai--jailed (file-name-as-directory temporary-file-directory) '("hax"))))
     (ert-info ("jailed should pass the project as a real path without a trailing slash")
       (should (equal (concat "PROJECT=" (directory-file-name (file-truename temporary-file-directory)))
                      (+ai-jail-test--param "PROJECT" command))))
     (ert-info ("jailed should pass HOME as a real path, since Seatbelt matches resolved paths")
       (should (equal (concat "HOME=" (directory-file-name (file-truename "~")))
                      (+ai-jail-test--param "HOME" command))))
     (ert-info ("jailed should pass the temp dir as a real path")
       (should (equal (concat "TMPDIR=" (directory-file-name (file-truename temporary-file-directory)))
                      (+ai-jail-test--param "TMPDIR" command))))
     (ert-info ("every profile param should follow its own -D flag")
       (should (seq-every-p (lambda (i) (equal "-D" (nth (1- i) command)))
                            (seq-filter (lambda (i) (string-match-p "\\`[A-Z]+=" (nth i command)))
                                        (number-sequence 0 (1- (length command))))))))))

(ert-deftest +ai--jailed/passes-through-when-disabled ()
  (let ((+ai-jail-enabled nil))
    (cl-letf (((symbol-function '+ai--jail-available-p) (lambda () t)))
      (ert-info ("jailed should leave the command untouched when jailing is off")
        (should (equal '("hax") (+ai--jailed temporary-file-directory '("hax"))))))))

(ert-deftest +ai--jailed/warns-and-passes-through-without-sandbox-exec ()
  (let ((+ai-jail-enabled t)
        (+ai--jail-warned nil)
        messages)
    (cl-letf (((symbol-function '+ai--jail-available-p) (lambda () nil))
              ((symbol-function 'message) (lambda (&rest args) (push (apply #'format args) messages))))
      (ert-info ("jailed should run the agent as is when the OS has no sandbox-exec")
        (should (equal '("hax") (+ai--jailed temporary-file-directory '("hax")))))
      (+ai--jailed temporary-file-directory '("pi")))
    (ert-info ("jailed should say once that agents run unjailed")
      (should (equal 1 (length messages))))))

(ert-deftest +ai--jailed/leaves-remote-projects-alone ()
  (+ai-jail-test--available
   (ert-info ("jailed should not wrap agents that run on a remote host")
     (should (equal '("pi") (+ai--jailed "/ssh:host:/srv/repo/" '("pi")))))))

;;; the profile

(defun +ai-jail-test--profile-text ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "modules/my/ai/agent.sb" default-directory))
    (buffer-string)))

(ert-deftest +ai-jail-profile/is-the-file-next-to-jail-el ()
  (ert-info ("the default profile should be agent.sb from the live config, not a project copy")
    (should (equal (expand-file-name "modules/my/ai/agent.sb" default-directory)
                   (default-value '+ai-jail-profile)))))

(ert-deftest +ai-jail-profile/keeps-its-guards ()
  (let ((text (+ai-jail-test--profile-text)))
    (pcase-dolist (`(,regexp . ,what)
                   '(("(deny file-write\\*)" . "deny writes by default")
                     ("(subpath (param \"PROJECT\"))" . "allow writes in the project")
                     ("(subpath (project \"/\\.git\"))" . "deny writes to git state, hooks and config included")
                     ("(subpath (project \"/\\.pi\"))" . "deny writes to the project's pi inputs")
                     ("AGENTS" . "deny writes to AGENTS.md")
                     ("dir-locals" . "deny writes to dir-locals Emacs would evaluate")
                     ("(subpath (home \"/\\.ssh\"))" . "deny reading ssh keys")
                     ("secrets" . "deny reading secrets.el")))
      (ert-info ((format "agent.sb should %s" what))
        (should (string-match-p regexp text))))))

(ert-deftest +ai-jail-profile/denies-after-it-allows ()
  (let ((text (+ai-jail-test--profile-text)))
    (ert-info ("guard denials should follow the allow list, since the last matching rule wins")
      (should (< (string-match "(allow file-write\\*" text)
                 (string-match "(subpath (project \"/\\.git\"))" text))))))

;;; launchers

(ert-deftest +ai--start-pi/starts-pilish-jailed ()
  (let (seen
        (pilish-executable '("pi"))
        (pilish-extra-args nil)
        (pilish-project-trust-policy 'default))
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'pilish) (lambda (&rest _) (setq seen pilish-executable)))
               ((symbol-function '+ai--prefill-pilish-input) #'ignore))
       (+ai--start-pi temporary-file-directory 'unchanged)))
    (ert-info ("start-pi should hand pilish a sandbox-exec command that ends in pi")
      (should (equal "sandbox-exec" (car seen)))
      (should (equal "pi" (car (last seen)))))
    (ert-info ("start-pi should not change pilish-executable outside the launch")
      (should (equal '("pi") pilish-executable)))))

(ert-deftest +ai/hax/starts-hax-jailed ()
  (let (started)
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/hax"))
               ((symbol-function '+ai--project-root) (lambda () temporary-file-directory))
               ((symbol-function '+ai--live-buffer) #'ignore)
               ((symbol-function '+ai-trust--gate) (lambda (_agent _root on-allow) (funcall on-allow 'unchanged)))
               ((symbol-function '+ai--eat-in) (lambda (_root _name command) (setq started command))))
       (+ai/hax)))
    (ert-info ("hax should start through sandbox-exec, with hax as the jailed program")
      (should (equal "sandbox-exec" (car started)))
      (should (equal '("hax") (last started))))))

(ert-deftest +ai-trust--open-pi-for-trust/stays-unjailed ()
  (let (started)
    (+ai-jail-test--available
     (cl-letf (((symbol-function '+ai--eat-in) (lambda (_root _name command) (setq started command)))
               ((symbol-function 'message) #'ignore))
       (+ai-trust--open-pi-for-trust temporary-file-directory)))
    (ert-info ("the pi opened to record a trust decision must write trust.json, so it runs outside the jail")
      (should (equal '("pi") started)))))

;;; network

(ert-deftest +ai--jailed/network-is-open-unless-asked ()
  (+ai-jail-test--available
   (ert-info ("jailed should leave the network open by default")
     (should (equal "NETWORK=open" (+ai-jail-test--param "NETWORK" (+ai--jailed temporary-file-directory '("pi"))))))
   (ert-info ("jailed should restrict to loopback when asked for local")
     (should (equal "NETWORK=local"
                    (+ai-jail-test--param "NETWORK" (+ai--jailed temporary-file-directory '("pi") "local")))))
   (ert-info ("jailed should refuse a network mode the profile does not know")
     (should-error (+ai--jailed temporary-file-directory '("pi") "lan")))))

(ert-deftest +ai--pi-network/local-model-gets-loopback-only ()
  (pcase-dolist (`(,model . ,expected) '(("local" . "local") ("claude" . "open") (nil . "open")))
    (ert-info ((format ":MODEL: %S should run with network %s" model expected))
      (should (equal expected (+ai--pi-network model))))))

(ert-deftest +ai--start-pi/local-model-runs-offline ()
  (let (seen
        (pilish-executable '("pi"))
        (pilish-extra-args nil)
        (pilish-project-trust-policy 'default))
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'pilish) (lambda (&rest _) (setq seen pilish-executable)))
               ((symbol-function '+ai--prefill-pilish-input) #'ignore))
       (+ai--start-pi temporary-file-directory 'unchanged :model "local")))
    (ert-info ("a :MODEL: local session should reach only llama-server on loopback")
      (should (equal "NETWORK=local" (+ai-jail-test--param "NETWORK" seen))))))

(ert-deftest +ai-jail-profile/blocks-sockets-that-act-as-you ()
  (let ((text (+ai-jail-test--profile-text)))
    (pcase-dolist (`(,regexp . ,what)
                   '(("(param \"NETWORK\") \"local\"" . "switch to loopback-only on NETWORK=local")
                     ("remote ip \"localhost:\\*\"" . "keep loopback open for llama-server")
                     ("emacs\\[0-9\\]" . "block the Emacs server socket, a way back into your Emacs")
                     ("Listeners" . "block the ssh-agent socket")
                     ("docker\\.sock" . "block the Docker socket")
                     ("launchservicesd" . "block LaunchServices, which starts apps outside the jail")
                     ("appleevents" . "block AppleEvents, which drive apps outside the jail")))
      (ert-info ((format "agent.sb should %s" what))
        (should (string-match-p regexp text))))))
