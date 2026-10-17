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

(defvar +ai-jail-test--started nil
  "What the last `+ai-jail-test--pilish' started pi with.")

(defun +ai-jail-test--seen (directory)
  "Stand-in for `pilish--start-process': what pi would start with in DIRECTORY."
  (list :executable pilish-executable :extra-args pilish-extra-args
        :policy pilish-project-trust-policy :directory directory))

(defun +ai-jail-test--pilish (&optional _session)
  "Stand-in for `pilish': start pi from a chat buffer, as pilish does, through the jail hook."
  (with-temp-buffer
    (setq major-mode 'pilish-chat-mode)
    (setq +ai-jail-test--started (+ai--pi-start-process-jailed #'+ai-jail-test--seen default-directory))))

(defmacro +ai-jail-test--in-chat-buffer (&rest body)
  "Run BODY in a buffer that passes for a pilish chat buffer, under plain pilish settings."
  `(with-temp-buffer
     (setq major-mode 'pilish-chat-mode)
     (let ((pilish-executable '("pi"))
           (pilish-extra-args '("-e" "ext.ts"))
           (pilish-project-trust-policy 'default))
       ,@body)))

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
       (let ((params (seq-take-while (lambda (arg) (not (equal "env" arg))) command)))
         (should (seq-every-p (lambda (i) (equal "-D" (nth (1- i) params)))
                              (seq-filter (lambda (i) (string-match-p "\\`[A-Z_]+=" (nth i params)))
                                          (number-sequence 0 (1- (length params)))))))))))

(ert-deftest +ai--jailed/passes-the-ssh-agent-socket-to-deny ()
  (let ((sock (make-temp-file "agent" nil ".sock")))
    (unwind-protect
        (+ai-jail-test--available
         (let ((process-environment (cons (concat "SSH_AUTH_SOCK=" sock) process-environment)))
           (ert-info ("jailed should pass SSH_AUTH_SOCK as a real path, so agent.sb can deny it")
             (should (equal (concat "SSH_AUTH_SOCK=" (file-truename sock))
                            (+ai-jail-test--param "SSH_AUTH_SOCK" (+ai--jailed temporary-file-directory '("pi")))))))
         (let ((process-environment (cons "SSH_AUTH_SOCK" process-environment)))
           (ert-info ("jailed should pass a path no socket uses when there is no ssh-agent")
             (should (equal "SSH_AUTH_SOCK=/nonexistent"
                            (+ai-jail-test--param "SSH_AUTH_SOCK" (+ai--jailed temporary-file-directory '("pi"))))))))
      (delete-file sock))))

(ert-deftest +ai--jailed/points-dependency-caches-at-jail-only-dirs ()
  (+ai-jail-test--available
   (let* ((command (+ai--jailed temporary-file-directory '("pi")))
          (env (member "env" command)))
     (ert-info ("jailed should start the agent through env, after the profile params")
       (should env))
     (pcase-dolist (`(,var . ,dir) '(("UV_CACHE_DIR" . "uv") ("GITLIBS" . "gitlibs")))
       (ert-info ((format "jailed should set %s to the jail's own %s cache" var dir))
         (should (member (format "%s=%s" var (expand-file-name dir +ai-jail-cache-dir)) env)))))))

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
                     ("(subpath (project \"/\\.worktrees\"))" . "deny writes to other tasks' worktrees")
                     ("(subpath (project \"/\\.agents\"))" . "deny writes to the project's .agents skills")
                     ("\\[Aa]\\[Gg]\\[Ee]\\[Nn]\\[Tt]\\[Ss]" . "deny writes to AGENTS.md in any case")
                     ("\\[Oo]\\[Vv]\\[Ee]\\[Rr]\\[Rr]\\[Ii]\\[Dd]\\[Ee]" . "deny writes to AGENTS.override.md")
                     ("\\[Cc]\\[Ll]\\[Aa]\\[Uu]\\[Dd]\\[Ee]" . "deny writes to CLAUDE.md in any case")
                     ("\\[Dd]\\[Ii]\\[Rr]-" . "deny writes to dir-locals Emacs would evaluate")
                     ("(subpath (home \"/\\.ssh\"))" . "deny reading ssh keys")
                     ("secrets" . "deny reading secrets.el")
                     ("\\[Gg]\\[Ii]\\[Tt](/|\\$)" . "deny writes to nested repos, whose config git runs for you")
                     ("git-credentials" . "deny reading git's plaintext credentials")
                     ("\\.docker/config\\.json" . "deny reading docker registry credentials")
                     ("(subpath (home \"/\\.cache/agent-jail\"))" . "allow writes only to the jail's own caches")))
      (ert-info ((format "agent.sb should %s" what))
        (should (string-match-p regexp text))))))

(ert-deftest +ai-jail-profile/leaves-shared-caches-read-only ()
  (let ((text (+ai-jail-test--profile-text)))
    (dolist (cache '("/.cache/uv" "/.m2/repository" "/.gitlibs"))
      (ert-info ((format "agent.sb should not allow writes to the shared %s, which unjailed tools run" cache))
        (should-not (string-match-p (regexp-quote (format "(home \"%s\")" cache)) text))))))

(defconst +ai-jail-test--temp-reallow "(allow file-write*\n  (subpath (param \"TMPDIR\"))")

(defun +ai-jail-test--position (regexp text &optional start)
  (or (string-match regexp text start) (error "%s not in agent.sb" regexp)))

(ert-deftest +ai-jail-profile/denies-by-name-only-outside-temp ()
  (let* ((text (+ai-jail-test--profile-text))
         (names (+ai-jail-test--position "(deny file-write\\*\n  (regex" text))
         (reallow (+ai-jail-test--position (regexp-quote +ai-jail-test--temp-reallow) text names)))
    (ert-info ("the temp dirs should be re-allowed after the denies by name, so tests can write repos and fixtures there")
      (should (< names reallow)))
    (ert-info ("the project's own .git deny should come after that re-allow")
      (should (< reallow (+ai-jail-test--position "(subpath (project \"/\\.git\"))" text))))))

(defun +ai-jail-test--name-regexps (text)
  "The regexes of agent.sb's deny-by-name block, as Emacs regexps."
  (let ((start (+ai-jail-test--position "(deny file-write\\*\n  (regex" text))
        regexps)
    (with-temp-buffer
      (insert (substring text start (+ai-jail-test--position (regexp-quote +ai-jail-test--temp-reallow) text start)))
      (goto-char (point-min))
      (while (re-search-forward "(regex #\"\\([^\"]+\\)\")" nil t)
        (push (replace-regexp-in-string "[()|]" "\\\\\\&" (match-string 1) t) regexps)))
    regexps))

(ert-deftest +ai-jail-profile/name-rules-match-every-spelling ()
  (let ((regexps (+ai-jail-test--name-regexps (+ai-jail-test--profile-text)))
        (case-fold-search nil))
    (pcase-dolist (`(,path . ,expected)
                   '(("/p/AGENTS.md" . t) ("/p/sub/agents.MD" . t) ("/p/AGENTS.override.md" . t)
                     ("/p/agents.OVERRIDE.md" . t) ("/p/CLAUDE.md" . t) ("/p/sub/Claude.md" . t)
                     ("/p/.dir-locals.el" . t) ("/p/.DIR-LOCALS-2.EL" . t) ("/p/.git/config" . t)
                     ("/p/sub/.GIT/hooks/pre-commit" . t) ("/p/.git" . t)
                     ("/p/AGENTS.md.bak" . nil) ("/p/docs/CLAUDE.md~" . nil) ("/p/.github/ci.yml" . nil)
                     ("/p/.gitignore" . nil) ("/p/README.md" . nil)))
      (ert-info ((format "agent.sb's name rules should%s deny writing %s" (if expected "" " not") path))
        (should (eq expected (and (seq-some (lambda (regexp) (string-match-p regexp path)) regexps) t)))))))

(ert-deftest +ai-jail-profile/denies-after-it-allows ()
  (let ((text (+ai-jail-test--profile-text)))
    (ert-info ("guard denials should follow the allow list, since the last matching rule wins")
      (should (< (string-match "(allow file-write\\*" text)
                 (string-match "(subpath (project \"/\\.git\"))" text))))))

;;; launchers

(ert-deftest +ai--start-pi/starts-pilish-jailed ()
  (let ((+ai-jail-test--started nil)
        (pilish-executable '("pi"))
        (pilish-extra-args nil)
        (pilish-project-trust-policy 'default))
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'pilish) #'+ai-jail-test--pilish)
               ((symbol-function '+ai--pi-jail-hooked-p) (lambda () t))
               ((symbol-function '+ai--prefill-pilish-input) #'ignore))
       (+ai--start-pi temporary-file-directory 'unchanged)))
    (ert-info ("start-pi should have pilish start a sandbox-exec command that ends in pi")
      (should (equal "sandbox-exec" (car (plist-get +ai-jail-test--started :executable))))
      (should (equal "pi" (car (last (plist-get +ai-jail-test--started :executable))))))
    (ert-info ("start-pi should not change pilish-executable outside the launch")
      (should (equal '("pi") pilish-executable)))))

(ert-deftest +ai--start-pi/refuses-when-pilish-cannot-be-jailed ()
  (let (started prepared)
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'pilish) (lambda (&rest _) (setq started t)))
               ((symbol-function '+ai--pi-jail-hooked-p) #'ignore))
       (ert-info ("start-pi should refuse when pilish no longer starts pi where the jail hooks in")
         (should-error (+ai--start-pi temporary-file-directory 'unchanged
                                      :prepare (lambda () (setq prepared t)))
                       :type 'user-error))))
    (ert-info ("a refused start should neither prepare a worktree nor start pi")
      (should-not (or started prepared)))))

(ert-deftest +ai--pi-jail-hooked-p/follows-pilish-defining-the-hooked-function ()
  (skip-unless (not (fboundp 'pilish--start-process)))
  (ert-info ("the jail hook should not count as installed while pilish has no pilish--start-process")
    (should-not (+ai--pi-jail-hooked-p)))
  (unwind-protect
      (progn
        (defalias 'pilish--start-process (lambda (directory) directory))
        (ert-info ("the jail hook should be on pilish--start-process as soon as pilish defines it")
          (should (+ai--pi-jail-hooked-p))))
    (fmakunbound 'pilish--start-process)
    (+ai--hook-pi-jail)))

;;; pi restarts

(ert-deftest +ai--pi-start-process-jailed/starts-the-session-jailed-with-its-flags ()
  (+ai-jail-test--available
   (+ai-jail-test--in-chat-buffer
    (let ((seen (let ((+ai--pi-next-launch (+ai--pi-launch-settings "/r/" 'untrusted-once "local")))
                  (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/"))))
      (ert-info ("a session start should run pi through sandbox-exec on its model's network")
        (should (equal "sandbox-exec" (car (plist-get seen :executable))))
        (should (equal "NETWORK=local" (+ai-jail-test--param "NETWORK" (plist-get seen :executable)))))
      (ert-info ("a session start should pass the model flags after your own extra args")
        (should (equal '("-e" "ext.ts" "--model" "qwopus-coder") (plist-get seen :extra-args))))
      (ert-info ("a session start should pass the review verdict's trust policy")
        (should (eq 'no-approve (plist-get seen :policy))))))))

(ert-deftest +ai--pi-start-process-jailed/reload-starts-pi-as-the-session-started ()
  (+ai-jail-test--available
   (+ai-jail-test--in-chat-buffer
    (let ((first (let ((+ai--pi-next-launch (+ai--pi-launch-settings "/r/" 'untrusted-once "local")))
                   (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/")))
          (reload (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/")))
      (ert-info ("/reload should restart pi jailed, with the network, model and trust flags it started with")
        (should (equal first reload)))))))

(ert-deftest +ai--pi-start-process-jailed/launch-survives-a-mode-change ()
  (+ai-jail-test--available
   (+ai-jail-test--in-chat-buffer
    (let ((first (let ((+ai--pi-next-launch (+ai--pi-launch-settings "/r/" 'unchanged "local")))
                   (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/"))))
      (kill-all-local-variables)
      (setq major-mode 'pilish-chat-mode)
      (ert-info ("a chat buffer that re-runs its mode should still restart pi as the session started")
        (should (equal first (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/"))))))))

(ert-deftest +ai--pi-start-process-jailed/jails-pi-that-pilish-starts-on-its-own ()
  (+ai-jail-test--available
   (+ai-jail-test--in-chat-buffer
    (let ((seen (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/elsewhere/")))
      (ert-info ("a pi pilish starts by itself (M-x pilish, a resumed session) should be jailed in its directory")
        (should (equal "sandbox-exec" (car (plist-get seen :executable))))
        (should (equal "PROJECT=/elsewhere" (+ai-jail-test--param "PROJECT" (plist-get seen :executable)))))
      (ert-info ("such a pi should get the open network and your own pilish flags")
        (should (equal "NETWORK=open" (+ai-jail-test--param "NETWORK" (plist-get seen :executable))))
        (should (equal '("-e" "ext.ts") (plist-get seen :extra-args)))
        (should (eq 'default (plist-get seen :policy))))))))

(ert-deftest +ai--pi-start-process-jailed/leaves-pilish-defaults-alone ()
  (+ai-jail-test--available
   (+ai-jail-test--in-chat-buffer
    (+ai--pi-start-process-jailed #'+ai-jail-test--seen "/r/")
    (ert-info ("starting pi jailed should not rewrite your pilish settings")
      (should (equal '("pi") (default-value 'pilish-executable)))
      (should (equal '("-e" "ext.ts") (default-value 'pilish-extra-args)))))))

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
  (let ((+ai-jail-test--started nil)
        (pilish-executable '("pi"))
        (pilish-extra-args nil)
        (pilish-project-trust-policy 'default))
    (+ai-jail-test--available
     (cl-letf (((symbol-function 'pilish) #'+ai-jail-test--pilish)
               ((symbol-function '+ai--pi-jail-hooked-p) (lambda () t))
               ((symbol-function '+ai--prefill-pilish-input) #'ignore))
       (+ai--start-pi temporary-file-directory 'unchanged :model "local")))
    (ert-info ("a :MODEL: local session should reach only llama-server on loopback")
      (should (equal "NETWORK=local" (+ai-jail-test--param "NETWORK" (plist-get +ai-jail-test--started :executable)))))))

(ert-deftest +ai-jail-profile/blocks-sockets-that-act-as-you ()
  (let ((text (+ai-jail-test--profile-text)))
    (pcase-dolist (`(,regexp . ,what)
                   '(("(param \"NETWORK\") \"local\"" . "switch to loopback-only on NETWORK=local")
                     ("remote ip \"localhost:8080\"" . "keep llama-server's port open on loopback")
                     ("emacs\\[0-9\\]" . "block the Emacs server socket, a way back into your Emacs")
                     ("Listeners" . "block the launchd ssh-agent socket")
                     ("(literal (param \"SSH_AUTH_SOCK\"))" . "block whatever ssh-agent SSH_AUTH_SOCK names")
                     ("docker\\.sock" . "block the Docker socket")
                     ("launchservicesd" . "block LaunchServices, which starts apps outside the jail")
                     ("appleevents" . "block AppleEvents, which drive apps outside the jail")))
      (ert-info ((format "agent.sb should %s" what))
        (should (string-match-p regexp text))))
    (ert-info ("agent.sb should not open every loopback port or name lookups to local-model sessions")
      (should-not (string-match-p "localhost:\\*\\|mDNSResponder" text)))))
