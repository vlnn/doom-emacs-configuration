;;; my/python/dape.el -*- lexical-binding: t; -*-

(add-hook! (python-mode python-ts-mode) (require 'dape))

(defun +dape--uv-debugpy-base ()
  `(modes (python-mode python-ts-mode)
    command "uv"
    command-args ("run" "python" "-m" "debugpy.adapter")
    :type "executable"
    :request "launch"
    :cwd dape-cwd-fn))

(defun +dape--program-config (file)
  `(,@(+dape--uv-debugpy-base) :program ,file))

(defun +dape--pytest-config (&rest targets)
  `(,@(+dape--uv-debugpy-base) :module "pytest" :args ,(vconcat targets)))

(defun +dape--current-pytest-target ()
  (concat (buffer-file-name) "::" (which-function)))

(defun +dape--test-file-p (file)
  (let ((name (file-name-nondirectory file)))
    (or (string-prefix-p "test_" name)
        (string-suffix-p "_test.py" name)
        (string-suffix-p "conftest.py" name))))

(defun +dape/debugpy-pytest-at-point ()
  (interactive)
  (dape (+dape--pytest-config (+dape--current-pytest-target))))

(defun +dape/continue-to-line ()
  (interactive)
  (dape-breakpoint-toggle)
  (dape-continue (dape--live-connection 'last t))
  (run-with-timer 0.5 nil #'dape-breakpoint-toggle))

(defun +dape/smart-debug ()
  "Run pytest under debugpy if the buffer looks like a test file, otherwise launch the file."
  (interactive)
  (let ((file (buffer-file-name)))
    (dape (if (+dape--test-file-p file)
              (+dape--pytest-config (+dape--current-pytest-target))
            (+dape--program-config file)))))

(after! dape
  (remove-hook 'dape-start-hook #'dape-info)
  (remove-hook 'dape-start-hook #'dape-repl)

  (set-popup-rule! "\\*dape-repl\\*"        :side 'bottom :height 0.25)
  (set-popup-rule! "\\*dape-info Scope\\*"  :side 'right  :width 0.2 :select nil)

  (setf (alist-get 'debugpy        dape-configs) (+dape--program-config 'dape-buffer-default)
        (alist-get 'debugpy-pytest dape-configs) (+dape--pytest-config)))

(map! :leader
      :desc "Debug test at point" "d t" #'+dape/debugpy-pytest-at-point
      :desc "Go to line"          "d g" #'+dape/continue-to-line
      :desc "Smart debug"         "d d" #'+dape/smart-debug)
