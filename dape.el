;; Dape debugger configuration for Doom Emacs
(use-package! dape
  :ensure t
  :defer t
  :custom
  (dape-buffer-window-arrangement nil)
  (dape-info-hide-mode-line t)
  :config
  (dape-breakpoint-global-mode)
  (map! :leader :prefix "d" :desc "Run forward to cursor" "f" #'dape-until))

;;; Configuration
(defconst my/dape-window-width 0.25)
(defconst my/dape-window-height 0.25)
(defconst my/dape-setup-delay 0.3)

;;; Debug configurations
(defun my/debugpy-config (runner)
  "Create debugpy config for RUNNER (poetry/uv)."
  `(,(intern (format "debugpy-%s" runner))
    modes (python-mode python-ts-mode)
    command ,runner
    command-args ("run" "python" "-m" "debugpy.adapter")
    :type "executable"
    :request "launch"
    :cwd dape-cwd-fn
    :program dape-buffer-default
    :console "integratedTerminal"
    :justMyCode nil))

(defun my/pytest-config (runner)
  "Create pytest config for RUNNER (poetry/uv)."
  (if (eq runner 'uv)
      `(debugpy-uv-pytest
        modes (python-mode python-ts-mode)
        command "uv"
        command-args ("run" "python" "-m" "debugpy" "--listen" "5678" 
                      "--wait-for-client" "-m" "pytest" dape-buffer-default)
        command-cwd ,(lambda () (project-root (project-current)))
        port 5678
        :type "python"
        :request "attach"
        :connect (:host "localhost" :port 5678))
    `(debugpy-poetry-pytest
      modes (python-mode python-ts-mode)
      command "poetry"
      command-args ("run" "python" "-m" "debugpy" "--listen" "localhost:0"
                    "--wait-for-client" "-m" "pytest" "-s")
      :type "executable"
      :request "launch"
      :cwd dape-cwd-fn
      :program dape-buffer-default
      :stopOnEntry t)))

(defun my/add-debug-configs ()
  "Register all debug configurations."
  (dolist (runner '("poetry" "uv"))
    (pushnew! dape-configs (my/debugpy-config runner)))
  (dolist (runner '(poetry uv))
    (pushnew! dape-configs (my/pytest-config runner))))

;;; Buffer utilities
(defun my/dape-buffer (info-type)
  "Get dape buffer for INFO-TYPE."
  (get-buffer (if (string= info-type "repl")
                  "*dape-repl*"
                (format "*dape-info %s*" info-type))))

;;; Window display
(defun my/side-window-config (side slot &optional width height)
  "Create side window config."
  `((side . ,side) (slot . ,slot)
    ,@(when width `((window-width . ,width)))
    ,@(when height `((window-height . ,height)))))

(defun my/display-side-window (buffer side slot &optional width height)
  "Display BUFFER in side window."
  (when buffer
    (display-buffer-in-side-window 
     buffer (my/side-window-config side slot width height))))

(defun my/display-right (buffer slot)
  "Display BUFFER on right side at SLOT."
  (my/display-side-window buffer 'right slot my/dape-window-width))

(defun my/display-bottom (buffer)
  "Display BUFFER at bottom."
  (my/display-side-window buffer 'bottom -1 nil my/dape-window-height))

;;; Window management
(defun my/toggle-sides ()
  "Toggle side windows if available."
  (when (fboundp 'window-toggle-side-windows)
    (window-toggle-side-windows)))

(defun my/setup-info-windows ()
  "Setup debug info windows."
  (let ((windows '(("Stack" . -3) ("Scope" . -2) 
                   ("Breakpoints" . -1) ("Threads" . 0))))
    (dolist (window windows)
      (my/display-right (my/dape-buffer (car window)) (cdr window)))))

(defun my/setup-windows ()
  "Setup all dape windows."
  (my/toggle-sides)
  (my/setup-info-windows)
  (my/display-bottom (my/dape-buffer "repl")))

(defun my/delayed-setup ()
  "Setup windows after delay."
  (run-with-timer my/dape-setup-delay nil #'my/setup-windows))

;;; Individual window commands
(defun my/show-window (info-type slot side)
  "Show specific dape window."
  (if (eq side 'bottom)
      (my/display-bottom (my/dape-buffer info-type))
    (my/display-right (my/dape-buffer info-type) slot)))

(defun my/show-dape-stack () (interactive) (my/show-window "Stack" -3 'right))
(defun my/show-dape-scope () (interactive) (my/show-window "Scope" -2 'right))
(defun my/show-dape-breakpoints () (interactive) (my/show-window "Breakpoints" -1 'right))
(defun my/show-dape-threads () (interactive) (my/show-window "Threads" 0 'right))
(defun my/show-dape-repl () (interactive) (my/show-window "repl" nil 'bottom))

;;; Setup hooks
(after! dape
  (my/add-debug-configs)
  (add-hook 'dape-on-start-hooks #'dape-info)
  (add-hook 'dape-on-start-hooks #'dape-repl)
  (add-hook 'dape-on-start-hooks #'my/delayed-setup)
  (add-hook 'dape-on-stopped-hooks #'my/delayed-setup)
  (add-hook 'dape-on-disconnect-hooks #'my/toggle-sides))
