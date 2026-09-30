;;; ai.el -*- lexical-binding: t; -*-

;;; llama-server

(defconst +ai-llama-host "127.0.0.1:8080")
(defconst +ai-fast-model 'qwopus-coder)
(defconst +ai-think-model 'qwopus-reason)

(defconst +ai-llama-fallback-models
  '(qwopus-reason qwopus-coder qwopus-fast gpt-oss-20b))

(defun +ai--llama-url (path)
  (concat "http://" +ai-llama-host path))

(defun +ai--llama-model-ids (json)
  (mapcar (lambda (m) (intern (alist-get 'id m)))
          (alist-get 'data json)))

(defun +ai--llama-fetch-models ()
  (with-current-buffer (url-retrieve-synchronously (+ai--llama-url "/v1/models") t t 2)
    (goto-char url-http-end-of-headers)
    (let ((json-key-type 'symbol)
          (json-array-type 'list))
      (+ai--llama-model-ids (json-read)))))

(defun +ai--llama-models ()
  (or (ignore-errors (+ai--llama-fetch-models))
      +ai-llama-fallback-models))

(defun +ai--llama-backend ()
  (gptel-make-openai "llama-server"
    :host +ai-llama-host
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :key "llama-server"
    :stream t
    :models (+ai--llama-models)))

(defun +ai--install-llama-backend ()
  (setq gptel-backend (+ai--llama-backend)
        gptel-quick-backend gptel-backend
        gptel-magit-backend gptel-backend))

(defun +ai/refresh-llama-models ()
  "Re-read the served model list after editing llama-server/config.ini."
  (interactive)
  (+ai--install-llama-backend)
  (message "gptel models: %s" (gptel-backend-models gptel-backend)))

(set-popup-rule! "^\\*llama-server\\*$"
  :side 'right :size 0.4 :select t :quit nil :ttl nil)

;;; gptel

(after! gptel
  (setq gptel-model +ai-fast-model
        gptel-include-reasoning 'ignore
        gptel-rewrite-default-action 'dispatch)
  (+ai--install-llama-backend)
  (setf (alist-get 'review gptel-directives)
        "You review code. Flag non-idiomatic constructs, missing or weak test cases, oversized functions, and asserts lacking explanation strings. Be terse."
        (alist-get 'plan gptel-directives)
        "You plan TDD work. Given a function or feature, list the failing tests to write first (in order) and the small named functions to implement. No code yet."
        (alist-get 'test gptel-directives)
        "You write pytest tests. Parametrize aggressively, mock with pytest-mock (never unittest.mock), give every assert an explanation string of the form 'X should Y'. Small named helpers over fixtures with logic. Output only code."
        (alist-get 'clojure gptel-directives)
        "You answer about idiomatic Clojure. Prefer threading macros, destructuring, and the seq library. Show minimal examples."))

;;; gptel-quick — SPC o l e

(after! gptel
  (setq gptel-quick-model +ai-fast-model
        gptel-quick-word-count 24
        gptel-quick-timeout 60))

;;; annotated explanation — SPC o l E

(defconst +ai-annotate-directive
  "You receive source code, each line prefixed by its number and a colon. For every non-blank line output exactly one line of the form `N: explanation`, N being that line's number, saying what the line does in under 80 characters, naming its steps in evaluation order. In stack languages the top of stack is consumed first. Output nothing else: no code, no prose, no fences.")

(defconst +ai-annotate-width 60)

(defun +ai--annotate-text ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'defun t)))

(defun +ai--annotate-number-lines (text)
  (string-join (seq-map-indexed (lambda (line i) (format "%d: %s" (1+ i) line))
                                (split-string text "\n"))
               "\n"))

(defun +ai--annotate-parse (response)
  (let (result)
    (dolist (line (split-string response "\n" t))
      (when (string-match "\\`\\s-*\\([0-9]+\\)[.:)]\\s-*\\(.*\\)\\'" line)
        (push (cons (string-to-number (match-string 1 line))
                    (string-trim (match-string 2 line)))
              result)))
    (nreverse result)))

(defun +ai--annotate-explanation (n parsed)
  (string-join (mapcar #'cdr (seq-filter (lambda (e) (= (car e) n)) parsed)) " "))

(defun +ai--annotate-wrap (text)
  (with-temp-buffer
    (insert text)
    (let ((fill-column +ai-annotate-width))
      (fill-region (point-min) (point-max)))
    (split-string (buffer-string) "\n" t)))

(defun +ai--annotate-indentation (line)
  (if (string-match "\\`\\s-*" line) (match-string 0 line) ""))

(defun +ai--annotate-comment-lines (n text indent)
  (let* ((prefix (concat indent (string-trim (or comment-start "#")) " "))
         (label (format "%d. " n))
         (pad (make-string (length label) ?\s))
         (lines (+ai--annotate-wrap text)))
    (cons (concat prefix label (car lines))
          (mapcar (lambda (l) (concat prefix pad l)) (cdr lines)))))

(defun +ai--annotate-line (line n parsed)
  (let ((explanation (+ai--annotate-explanation n parsed)))
    (if (string-empty-p explanation)
        line
      (string-join (append (+ai--annotate-comment-lines
                            n explanation (+ai--annotate-indentation line))
                           (list line))
                   "\n"))))

(defun +ai--annotate-render (source parsed)
  (string-join (seq-map-indexed (lambda (line i) (+ai--annotate-line line (1+ i) parsed))
                                (split-string source "\n"))
               "\n"))

(defun +ai--annotate-buffer (mode)
  (with-current-buffer (get-buffer-create "*gptel-annotate*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p mode)
        (delay-mode-hooks (funcall mode)))
      (insert "Explaining...")
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'quit-window))
    (current-buffer)))

(defun +ai--annotate-show (buffer source response)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (+ai--annotate-render source (+ai--annotate-parse response))))))

(defun +ai--annotate-callback (source buffer)
  (lambda (response info)
    (pcase response
      ((pred stringp) (+ai--annotate-show buffer source response))
      ('nil (message "Annotate failed: %s" (plist-get info :status))))))

(defun +ai/annotate ()
  "Show the region or defun at point with a numbered explanation per line."
  (interactive)
  (let* ((source (+ai--annotate-text))
         (buffer (+ai--annotate-buffer major-mode))
         (gptel-model +ai-fast-model))
    (pop-to-buffer buffer)
    (gptel-request (+ai--annotate-number-lines source)
      :system +ai-annotate-directive
      :callback (+ai--annotate-callback source buffer))))

(set-popup-rule! "^\\*gptel-annotate\\*$"
  :side 'right :size 0.5 :select t :quit t :ttl nil)

(map! :leader :desc "Explain annotated" "o l E" #'+ai/annotate)

;;; gptel-magit — commit messages and diff explanations

(defun +ai--gptel-magit-require-staged (fn &rest args)
  (if (string-empty-p (magit-git-output "diff" "--cached"))
      (user-error "Nothing staged; stage the changes you want described")
    (apply fn args)))

(after! gptel-magit
  (setq gptel-magit-model +ai-fast-model
        gptel-magit-body-length 72)
  (advice-add 'gptel-magit--generate :around #'+ai--gptel-magit-require-staged))

(set-popup-rule! "^\\*gptel-magit diff-explain\\*$"
  :side 'right :size 0.4 :select t :quit 'current :ttl nil)

;;; agents

(use-package! aider
  :init
  (map! :leader :desc "aider" "1" #'aider-transient-menu)
  :config
  (require 'aider-doom)
  (setq aider-program "cecli"
        aider-args (list "--model" (format "openai/%s" +ai-fast-model)
                         "--openai-api-base" (+ai--llama-url "/v1")
                         "--openai-api-key" "llama-server"
                         "--no-show-model-warnings"))
  (set-popup-rule! "^\\*aider"   :quit nil)
  (set-popup-rule! "^\\*Python\\*" :quit nil))

(use-package! ai-code
  :config
  (ai-code-set-backend 'aider)
  (setq ai-code-menu-layout 'two-columns
        ai-code-auto-test-type 'ask-me)
  (map! "C-c a" #'ai-code-menu)
  (ai-code-prompt-filepath-completion-mode 1)
  (after! evil  (ai-code-backends-infra-evil-setup))
  (after! magit (ai-code-magit-setup-transients)))

(use-package! opencode
  :init
  (map! :leader :desc "opencode" "2" #'opencode)
  :config
  (setq opencode-default-model "anthropic/claude-sonnet-4-6")
  (set-popup-rule! "^\\*opencode" :side 'right :size 0.4 :select t :quit nil :ttl nil))
