;;; le-gpt-core.el --- Core functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; 


;;; Code:

(require 'savehist)

;; Core configuration variables
(defvar le-gpt--script-path
  (expand-file-name "le-gpt.py" (file-name-directory (or load-file-name buffer-file-name)))
  "The path to the Python script used by gpt.el.")

(defcustom le-gpt-model "claude-sonnet-4-5"
  "The model to use."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-api-type 'anthropic
  "The type of API to use.  \='openai, \='anthropic, or \='deepseek."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Deepseek" deepseek)
                 (const :tag "Anthropic" anthropic))
  :group 'le-gpt)

(defcustom le-gpt-max-tokens 2000
  "The max_tokens value used with the chosen model."
  :type 'integer
  :group 'le-gpt)

(defcustom le-gpt-temperature 0
  "The temperature value used with the chosen model."
  :type 'float
  :group 'le-gpt)

(defcustom le-gpt-openai-key "NOT SET"
  "The OpenAI API key to use."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-anthropic-key "NOT SET"
  "The Anthropic API key to use."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-deepseek-key "NOT SET"
  "The Deepseek API key to use."
  :type 'string
  :group 'le-gpt)


(defcustom le-gpt-python-path "python"
  "The path to your python executable."
  :type 'string
  :group 'le-gpt)

(defcustom le-gpt-model-list '(("GPT-5.1" . (openai . "gpt-5.1"))
                               ("GPT-5 mini" . (openai . "gpt-5-mini"))
                               ("GPT-5 nano" . (openai . "gpt-5-nano"))
                               ("Claude 4.5 Haiku" . (anthropic . "claude-haiku-4-5"))
                               ("Claude 4.5 Sonnet" . (anthropic . "claude-sonnet-4-5"))
                               ("Claude 4.5 Opus" . (anthropic . "claude-opus-4-5"))
                               ("DeepSeekV3" . (deepseek . "deepseek-chat"))
                               ("DeepSeekR1" . (deepseek . "deepseek-reasoner")))
  "List of available models with their API types and model names."
  :type '(alist :key-type string :value-type (cons symbol string))
  :group 'le-gpt)


;; Core process management functions

(defvar le-gpt--current-process nil
  "The currently running GPT process.")

(defvar le-gpt--process-interrupted nil
  "Flag to track if the current process was interrupted.")

(defvar le-gpt--current-timer nil
  "The currently running timer.")

(defun le-gpt--start-timer (process)
  "Set timer to run every second and print message if PROCESS is still running."
  (run-with-timer 1 1
                  (lambda (timer-object)
                    (when (process-live-p timer-object)
                      (font-lock-update)
                      (message "Le GPT: Running...")))
                  process))

(defun le-gpt--process-filter (process output)
  "Filter function for GPT process output.
Only insert output if process hasn't been interrupted."
  (unless le-gpt--process-interrupted
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (goto-char (point-max))
        (insert output)))))

(defun le-gpt--create-system-file (instructions)
  "Write INSTRUCTIONS to a temp file and return the path."
  (let ((temp-file (make-temp-file "le-gpt-system")))
    (with-temp-file temp-file
      (insert instructions))
    temp-file))

(defun le-gpt--make-process (prompt-file output-buffer &optional system-instructions)
  "Create a GPT process with PROMPT-FILE, OUTPUT-BUFFER, and optional SYSTEM-INSTRUCTIONS."
  (setq le-gpt--process-interrupted nil)
  (let* ((api-key (if (eq le-gpt-api-type 'openai)
                      le-gpt-openai-key
                    (if (eq le-gpt-api-type 'anthropic)
                        le-gpt-anthropic-key
                      le-gpt-deepseek-key)))
         (api-type-str (symbol-name le-gpt-api-type))
         (system-file (when system-instructions
                        (le-gpt--create-system-file system-instructions)))
         (command (append (list le-gpt-python-path
                                le-gpt--script-path
                                prompt-file api-key le-gpt-model
                                (number-to-string le-gpt-max-tokens)
                                (number-to-string le-gpt-temperature) api-type-str)
                          (when system-file (list "--system" system-file))))
         (process (make-process
                   :name "le-gpt-process"
                   :buffer output-buffer
                   :command command
                   :connection-type 'pipe
                   :filter #'le-gpt--process-filter))
         (timer (le-gpt--start-timer process)))
    (setq le-gpt--current-process process)
    (le-gpt--set-process-sentinel process timer prompt-file system-file)
    process))

(defun le-gpt--set-process-sentinel (process timer prompt-file &optional system-file)
  "Set a function to run when the PROCESS finishes or fails.
Cleans up PROMPT-FILE and optional SYSTEM-FILE on success."
  (set-process-sentinel
   process
   (lambda (proc status)
     (when (memq (process-status proc) '(exit signal))
       (cancel-timer timer)
       (setq le-gpt--current-process nil)
       (setq le-gpt--process-interrupted nil)
       (if (zerop (process-exit-status proc))
           (progn
             (delete-file prompt-file)
             (when system-file (delete-file system-file))
             (message "Le GPT: Finished successfully."))
         (message "Le GPT: Failed: %s" status))))))


(defun le-gpt-interrupt ()
  "Interrupt the currently running GPT process."
  (interactive)
  (if (and le-gpt--current-process
           (process-live-p le-gpt--current-process))
      (progn
        (setq le-gpt--process-interrupted t)
        (interrupt-process le-gpt--current-process)
        (when le-gpt--current-timer
          (cancel-timer le-gpt--current-timer)
          (setq le-gpt--current-timer nil))
        (message "Le GPT: Process interrupted"))
    (message "Le GPT: No running process to interrupt")))

;; Core utility functions
(defun le-gpt--create-prompt-file (input)
  "Create a temporary file containing the prompt string from INPUT."
  (let ((temp-file (make-temp-file "le-gpt-prompt"))
        (content (if (bufferp input)
                     (with-current-buffer input (buffer-string))
                   input)))
    (with-temp-file temp-file
      (insert content))
    (message "Le GPT: Prompt written to %s" temp-file)
    temp-file))


(defun le-gpt--completing-read-space (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Read string in minibuffer with completion, treating space literally.
Arguments PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD have same meaning as in `completing-read'."
  (let ((minibuffer-local-completion-map
         (let ((map (copy-keymap minibuffer-local-completion-map)))
           (define-key map " " 'self-insert-command)
           map)))
    (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method)))

;;;###autoload
(defun le-gpt-switch-model ()
  "Switch between OpenAI, Anthropic, and Deepseek models."
  (interactive)
  (let* ((choice (completing-read "Choose model: " (mapcar #'car le-gpt-model-list) nil t))
         (model-info (cdr (assoc choice le-gpt-model-list))))
    (setq le-gpt-api-type (car model-info)
          le-gpt-model (cdr model-info))
    (message "Switched to %s model: %s" (car model-info) (cdr model-info))))

;; Command reading
(defun le-gpt--read-command ()
  "Read a GPT command from the user with history and completion."
  (let ((cmd (le-gpt--completing-read-space "Command: " le-gpt--command-history nil nil nil 'le-gpt--command-history)))
    (if (string-equal cmd "n/a")
        ""
      (string-trim cmd))))

;; Command history management
(defvar le-gpt--command-history nil
  "A list of GPT commands that have been entered by the user.")

(add-to-list 'savehist-additional-variables 'le-gpt--command-history)

(defun le-gpt-display-command-history ()
  "Display the `le-gpt--command-history' in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*GPT Command History*")
    (erase-buffer)
    (insert (mapconcat #'identity le-gpt--command-history "\n"))
    (switch-to-buffer (current-buffer))))

(defun le-gpt-clear-command-history ()
  "Clear the `le-gpt--command-history' list."
  (interactive)
  (setq le-gpt--command-history nil)
  (message "GPT command history cleared."))

(defun le-gpt-export-command-history (file)
  "Export the `le-gpt--command-history' to FILE."
  (interactive "Export le-gpt--command-history to file: ")
  (with-temp-file file
    (dolist (cmd le-gpt--command-history)
      (insert (format "%s\n" cmd)))))

;; System instructions
(declare-function le-gpt-snippets--get-combined "le-gpt-snippets")

(defun le-gpt--build-system-instructions (task-instructions)
  "Combine enabled snippets with TASK-INSTRUCTIONS."
  (let ((snippets (and (featurep 'le-gpt-snippets)
                       (le-gpt-snippets--get-combined))))
    (if (and snippets task-instructions)
        (concat snippets "\n\n" task-instructions)
      (or snippets task-instructions))))

(provide 'le-gpt-core)

;;; le-gpt-core.el ends here
