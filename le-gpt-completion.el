;;; le-gpt-completion.el --- Completion functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-context)

(defcustom le-gpt-complete-at-point-instructions "Provide a short completion to be inserted at <cursor>. Only provide the completion, no commentary, no quotes, no code blocks. Your response will directly be inserted."
  "The instructions for gpt to perform completion at point without any noise."
  :type 'string
  :group 'le-gpt)

(defun le-gpt-completion-at-point (use-context)
  "Get completion from GPT based on buffer content up to point.
If USE-CONTEXT is non-nil, prompt for context files.
Pending context is always included if present.
The generated completion is displayed directly in buffer."
  (let* ((start-point (point))
         (buffer-content (buffer-substring-no-properties (point-min) start-point))
         (buffer-rest (buffer-substring-no-properties start-point (point-max)))
         (context (le-gpt--get-all-context use-context))
         (prompt (concat (when context (concat "User:\n\n" context))
                         "User: " buffer-content "<cursor>" buffer-rest))
         (prompt-file (le-gpt--create-prompt-file prompt))
         (system-instructions (le-gpt--build-system-instructions le-gpt-complete-at-point-instructions))
         (insertion-marker (make-marker))
         (process (le-gpt--make-process prompt-file nil system-instructions)))
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (when (and (not le-gpt--process-interrupted)
                                             (eq proc le-gpt--current-process)
                                             (marker-buffer insertion-marker))
                                    (save-excursion
                                      (goto-char insertion-marker)
                                      (insert string)
                                      (set-marker insertion-marker (point))))))))

(provide 'le-gpt-completion)
;;; le-gpt-completion.el ends here
