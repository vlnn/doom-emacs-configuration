;;; dape.el -*- lexical-binding: t; -*-

(add-hook! (python-mode python-ts-mode) (require 'dape))

(after! dape
  (remove-hook 'dape-start-hook #'dape-info)
  (remove-hook 'dape-start-hook #'dape-repl)

  (set-popup-rule! "\\*dape-repl\\*"        :side 'bottom :height 0.25)
  (set-popup-rule! "\\*dape-info Scope\\*"  :side 'right  :width 0.2 :select nil)

  (let ((uv-debugpy `(modes (python-mode python-ts-mode)
                      command "uv"
                      command-args ("run" "python" "-m" "debugpy.adapter")
                      :type "executable"
                      :request "launch"
                      :cwd dape-cwd-fn)))
    (setf (alist-get 'debugpy        dape-configs)
          `(,@uv-debugpy :program dape-buffer-default))
    (setf (alist-get 'debugpy-pytest dape-configs)
          `(,@uv-debugpy :module "pytest" :args []))))

(defun +dape--uv-debugpy-base ()
  `(modes (python-mode python-ts-mode)
    command "uv"
    command-args ("run" "python" "-m" "debugpy.adapter")
    :type "executable"
    :request "launch"
    :cwd ,(funcall dape-cwd-fn)))

(defun +dape--current-pytest-target ()
  (concat (buffer-file-name) "::" (which-function)))

(defun +dape--test-file-p (file)
  (let ((name (file-name-nondirectory file)))
    (or (string-prefix-p "test_" name)
        (string-suffix-p "_test.py" name)
        (string-suffix-p "conftest.py" name))))

(defun +dape/debugpy-pytest-at-point ()
  (interactive)
  (dape `(,@(+dape--uv-debugpy-base)
          :module "pytest"
          :args ,(vector (+dape--current-pytest-target)))))

(defun +dape/continue-to-line ()
  (interactive)
  (dape-breakpoint-toggle)
  (dape-continue (dape--live-connection 'last t))
  (run-with-timer 0.5 nil (lambda () (dape-breakpoint-toggle))))

(defun +dape/smart-debug ()
  "Run pytest under debugpy if the buffer looks like a test file, otherwise launch the file."
  (interactive)
  (let ((file (buffer-file-name))
        (base (+dape--uv-debugpy-base)))
    (if (+dape--test-file-p file)
        (dape `(,@base :module "pytest"
                :args ,(vector (+dape--current-pytest-target))))
      (dape `(,@base :program ,file)))))

(map! :leader
      :desc "Debug test at point" "d t" #'+dape/debugpy-pytest-at-point
      :desc "Go to line"          "d g" #'+dape/continue-to-line
      :desc "Smart debug"         "d d" #'+dape/smart-debug)
