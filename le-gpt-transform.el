;;; le-gpt-transform.el --- Transform region functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'le-gpt-context)

(defcustom le-gpt-transform-region-instructions
  "You should transform what is inside <region>. Only change what was requested without anything else, e.g., no explanatory comments, no triple backticks. Your response will replace what is inside region as is."
  "The instruction to give gpt so that it performs the transformation as intended."
  :type 'string
  :group 'le-gpt)

(defun le-gpt-transform-region-with-prompt (use-context)
  "Transform the selected region.
Ask user for the transformation command and replace region with response.
If USE-CONTEXT is non-nil, select context files interactively."
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (region-content (buffer-substring-no-properties start end))
         (buffer-before (buffer-substring-no-properties (point-min) start))
         (buffer-after (buffer-substring-no-properties end (point-max)))
         (context (if use-context (le-gpt--get-context) nil))
         (command (le-gpt--read-command))
         (prompt (concat (when context (concat "User:\n\n" context))
                         "User: " command "\n"
                         "<region>" region-content "<region>" "\n"
                         le-gpt-transform-region-instructions "\n"
                         "GPTContext: " buffer-before "\n" buffer-after))
         (prompt-file (le-gpt--create-prompt-file prompt))
         (insertion-marker (make-marker))
         (process (le-gpt--make-process prompt-file nil)))
    (delete-region start end)
    (set-marker insertion-marker (point))
    (set-process-filter process (lambda (proc string)
                                  (ignore proc)
                                  (save-excursion
                                    (goto-char insertion-marker)
                                    (insert string)
                                    (set-marker insertion-marker (point)))))))

(provide 'le-gpt-transform)
;;; le-gpt-transform.el ends here
