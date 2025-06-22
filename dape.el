;; Dape debugger configuration for Doom Emacs
(use-package! dape
  :ensure t
  :defer t
  :custom
  (dape-buffer-window-arrangement nil)
  (dape-info-hide-mode-line t)
  :config
  (dape-breakpoint-global-mode))

;;; Configuration constants
(defconst my/dape-side-window-width 0.25
  "Width ratio for dape side windows.")

(defconst my/dape-bottom-window-height 0.15
  "Height ratio for dape bottom window.")

(defconst my/dape-window-setup-delay 0.3
  "Delay in seconds before setting up dape windows.")

;;; Poetry debug configurations
(defun my/add-poetry-debug-configs ()
  "Add poetry-specific debug configurations to dape."
  (my/add-poetry-debugpy-config)
  (my/add-poetry-pytest-config))

(defun my/add-poetry-debugpy-config ()
  "Add basic poetry debugpy configuration."
  (pushnew! dape-configs
            `(debugpy-poetry
              modes (python-mode python-ts-mode)
              command "poetry"
              command-args ("run" "python" "-m" "debugpy.adapter")
              :type "executable"
              :request "launch"
              :cwd dape-cwd-fn
              :program dape-buffer-default
              :stopOnEntry 't)))

(defun my/add-poetry-pytest-config ()
  "Add poetry pytest debug configuration."
  (pushnew! dape-configs
            `(debugpy-poetry-pytest
              modes (python-mode python-ts-mode)
              command "poetry"
              command-args ("run" "python" "-m" "debugpy" "--listen" "localhost:0"
                            "--wait-for-client" "-m" "pytest" "-s")
              :type "executable"
              :request "launch"
              :cwd dape-cwd-fn
              :program dape-buffer-default
              :stopOnEntry 't)))

(after! dape
  (my/add-poetry-debug-configs))

;;; Buffer management
(defun my/dape-buffer-name (info-type)
  "Generate dape buffer name for INFO-TYPE."
  (format "*dape-info %s*" info-type))

(defun my/get-dape-info-buffer (info-type)
  "Get dape info buffer by INFO-TYPE if it exists."
  (get-buffer (my/dape-buffer-name info-type)))

(defun my/get-dape-repl-buffer ()
  "Get the dape REPL buffer if it exists."
  (get-buffer "*dape-repl*"))

;;; Window display utilities
(defun my/make-side-window-config (side slot &optional width height)
  "Create window configuration for SIDE and SLOT with optional WIDTH or HEIGHT."
  (let ((config `((side . ,side) (slot . ,slot))))
    (when width
      (push `(window-width . ,width) config))
    (when height
      (push `(window-height . ,height) config))
    config))

(defun my/display-buffer-in-side (buffer side slot &optional width height)
  "Display BUFFER in side window with SIDE, SLOT, and optional WIDTH or HEIGHT."
  (when buffer
    (display-buffer-in-side-window
     buffer
     (my/make-side-window-config side slot width height))))

(defun my/display-dape-buffer-right (buffer slot)
  "Display BUFFER in right side window at SLOT position."
  (my/display-buffer-in-side buffer 'right slot my/dape-side-window-width))

(defun my/display-dape-buffer-bottom (buffer)
  "Display BUFFER in bottom side window."
  (my/display-buffer-in-side buffer 'bottom -1 nil my/dape-bottom-window-height))

;;; Window layout management
(defun my/toggle-side-windows-safely ()
  "Toggle side windows if the function is available."
  (when (fboundp 'window-toggle-side-windows)
    (window-toggle-side-windows)))

(defun my/clear-existing-side-windows ()
  "Clear any existing side windows."
  (my/toggle-side-windows-safely))

(defun my/display-debug-info-windows ()
  "Display all debug info windows on the right side."
  (let ((window-configs '(("Stack" . -3)
                          ("Scope" . -2)
                          ("Breakpoints" . -1)
                          ("Threads" . 0))))
    (dolist (config window-configs)
      (my/display-dape-buffer-right
       (my/get-dape-info-buffer (car config))
       (cdr config)))))

(defun my/display-repl-window ()
  "Display the dape REPL window at the bottom."
  (my/display-dape-buffer-bottom (my/get-dape-repl-buffer)))

(defun my/create-dape-window-layout ()
  "Create the complete window layout for dape buffers."
  (my/clear-existing-side-windows)
  (my/display-debug-info-windows)
  (my/display-repl-window))

;;; Main window setup functions
(defun my/setup-dape-windows ()
  "Setup all dape debug windows with proper layout after a delay."
  (run-with-timer my/dape-window-setup-delay nil #'my/create-dape-window-layout))

(defun my/cleanup-dape-windows ()
  "Clean up dape windows when debugging stops."
  (my/toggle-side-windows-safely))

;;; Individual window display commands
(defun my/show-dape-stack ()
  "Show dape stack window."
  (interactive)
  (my/display-dape-buffer-right (my/get-dape-info-buffer "Stack") -3))

(defun my/show-dape-scope ()
  "Show dape scope window."
  (interactive)
  (my/display-dape-buffer-right (my/get-dape-info-buffer "Scope") -2))

(defun my/show-dape-breakpoints ()
  "Show dape breakpoints window."
  (interactive)
  (my/display-dape-buffer-right (my/get-dape-info-buffer "Breakpoints") -1))

(defun my/show-dape-threads ()
  "Show dape threads window."
  (interactive)
  (my/display-dape-buffer-right (my/get-dape-info-buffer "Threads") 0))

(defun my/show-dape-repl ()
  "Show dape REPL window."
  (interactive)
  (my/display-dape-buffer-bottom (my/get-dape-repl-buffer)))

;;; Hook setup
(add-hook 'dape-on-start-hooks #'dape-info)
(add-hook 'dape-on-start-hooks #'dape-repl)
(add-hook 'dape-on-start-hooks #'my/setup-dape-windows)
(add-hook 'dape-on-stopped-hooks #'my/setup-dape-windows)
(add-hook 'dape-on-disconnect-hooks #'my/cleanup-dape-windows)
