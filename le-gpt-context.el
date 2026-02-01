;;; le-gpt-context.el --- Context functionality for le-gpt -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Core context functionality and main entry points.

;;; Code:

;;; Pending Context Queue
;; Allows users to accumulate context snippets that are automatically
;; included in the next GPT request.

(defvar le-gpt--pending-context nil
  "List of pending context items to include in next GPT request.
Each item is a plist with :content, :source, :lines, :timestamp.")

(defun le-gpt--pending-context-format-item (item)
  "Format a single pending context ITEM for display in prompt."
  (let ((source (plist-get item :source))
        (lines (plist-get item :lines))
        (content (plist-get item :content)))
    (format "Context from %s%s:\n```\n%s\n```"
            source
            (if lines (format " (lines %s)" lines) "")
            content)))

(defun le-gpt--pending-context-get-formatted ()
  "Get formatted string of all pending context items, or nil if empty."
  (when le-gpt--pending-context
    (mapconcat #'le-gpt--pending-context-format-item
               (reverse le-gpt--pending-context)
               "\n\n")))

(defun le-gpt--pending-context-clear ()
  "Clear all pending context items."
  (setq le-gpt--pending-context nil))

(defun le-gpt--pending-context-count ()
  "Return the number of pending context items."
  (length le-gpt--pending-context))

;;;###autoload
(defun le-gpt-context-add-region (start end)
  "Add the region from START to END as pending context.
The snippet will be automatically included in the next GPT request."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((content (buffer-substring-no-properties start end))
         (source (buffer-name))
         (start-line (line-number-at-pos start))
         (end-line (line-number-at-pos end))
         (lines (if (= start-line end-line)
                    (number-to-string start-line)
                  (format "%d-%d" start-line end-line)))
         (line-count (1+ (- end-line start-line)))
         (item (list :content content
                     :source source
                     :lines lines
                     :timestamp (current-time))))
    (push item le-gpt--pending-context)
    (message "Added %d line%s from %s (%d item%s pending)"
             line-count
             (if (= line-count 1) "" "s")
             source
             (le-gpt--pending-context-count)
             (if (= (le-gpt--pending-context-count) 1) "" "s"))))

;;;###autoload
(defun le-gpt-context-clear ()
  "Clear all pending context."
  (interactive)
  (let ((count (le-gpt--pending-context-count)))
    (le-gpt--pending-context-clear)
    (message "Cleared %d pending context item%s"
             count
             (if (= count 1) "" "s"))))

;;;###autoload
(defun le-gpt-context-show ()
  "Show current pending context in a buffer."
  (interactive)
  (if (null le-gpt--pending-context)
      (message "No pending context")
    (let ((buffer (get-buffer-create "*Le GPT Pending Context*")))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert (format "Pending Context (%d item%s):\n\n"
                        (le-gpt--pending-context-count)
                        (if (= (le-gpt--pending-context-count) 1) "" "s")))
        (insert (make-string 50 ?-) "\n\n")
        (dolist (item (reverse le-gpt--pending-context))
          (insert (format "Source: %s (lines %s)\n"
                          (plist-get item :source)
                          (plist-get item :lines)))
          (insert (make-string 30 ?-) "\n")
          (insert (plist-get item :content))
          (insert "\n\n" (make-string 50 ?-) "\n\n"))
        (goto-char (point-min))
        (read-only-mode 1))
      (display-buffer buffer))))

(require 'le-gpt-core)
(require 'le-gpt-context-utils)
(require 'le-gpt-context-history)
(require 'le-gpt-context-files)
(require 'le-gpt-context-buffers)

(defun le-gpt--get-context ()
  "Get the formatted context string based on the selected context."
  (let* ((selected-items (le-gpt--read-multiple-context-items))
         (selected-files (seq-filter (lambda (item)
                                       (eq (get-text-property 0 'context-type item) 'file))
                                     selected-items))
         (selected-buffers (seq-filter (lambda (item)
                                         (eq (get-text-property 0 'context-type item) 'buffer))
                                       selected-items))
         (context-string ""))

    ;; Save to history if we have selections
    (when selected-items
      (le-gpt--save-context-to-history selected-items))

    ;; Add file context
    (when selected-files
      (setq context-string
            (concat context-string
                    (format le-gpt--project-context-format
                            (mapconcat #'identity selected-files "\n")
                            (le-gpt--get-selected-files-contents selected-files)))))

    ;; Add buffer context
    (when selected-buffers
      (setq context-string
            (concat context-string
                    (format "Buffer Context:\n%s\n\nBuffer Contents:\n%s\n\n"
                            (mapconcat #'identity selected-buffers "\n")
                            (le-gpt--get-selected-buffers-contents selected-buffers)))))

    (when (not (string-empty-p context-string))
      context-string)))

(defun le-gpt--create-context-completions ()
  "Create completion candidates for context selection with rich metadata."
  (let ((completions '())
        (project-root (when (project-current)
                        (project-root (project-current))))
        (project-files (condition-case nil
                           (le-gpt--get-project-files)
                         (error nil)))
        (buffer-names (le-gpt--get-buffer-names)))

    ;; Add buffers with rich metadata (added first, will end up last)
    (dolist (buffer-name buffer-names)
      (let* ((buffer (get-buffer buffer-name))
             (candidate (propertize buffer-name
                                    'context-type 'buffer
                                    'context-name buffer-name)))
        (push (cons candidate `((type . buffer)
                                (name . ,buffer-name)
                                (buffer . ,buffer)))  ; Store minimal info, compute rest lazily
              completions)))

    ;; Add project files with MINIMAL metadata
    (dolist (file project-files)
      (let ((candidate (propertize file
                                   'context-type 'file
                                   'context-path file)))
        (push (cons candidate `((type . file)
                                (path . ,file)
                                (project-root . ,project-root)))  
              completions)))

    ;; Add history entries LAST (will end up first due to append)
    (setq completions (append (le-gpt--create-history-completions) completions))

    completions))

(defun le-gpt--get-context-annotations (completions)
  "Get annotation function with dynamic padding for perfect alignment.
Computes expensive metadata lazily only when annotation is displayed."
  (let ((max-name-length (apply #'max
                                (mapcar (lambda (comp) (length (car comp)))
                                        completions))))
    (lambda (candidate)
      (let* ((metadata (assoc-default candidate completions))
             (type (assoc-default 'type metadata))
             (current-length (length candidate))
             (padding (max 4 (- (+ max-name-length 4) current-length))))
        (cond
         ((eq type 'history)
          (let* ((timestamp (assoc-default 'timestamp metadata)))
            (format "%s%-8s %s"
                    (make-string padding ?\s)
                    "[History]"
                    (le-gpt--format-time-ago timestamp))))

         ((eq type 'file)
          ;; Compute file info lazily only when displaying annotation
          (let* ((path (assoc-default 'path metadata))
                 (project-root (assoc-default 'project-root metadata))
                 (full-path (expand-file-name path project-root))
                 (file-info (le-gpt--get-file-info-cached full-path))
                 (size (car file-info))
                 (modified (cdr file-info))
                 (ext (file-name-extension path)))
            (format "%s%-8s %-8s %-8s %s"
                    (make-string padding ?\s)
                    "[File]"
                    (le-gpt--format-file-size size)
                    (if ext (format "(%s)" ext) "")
                    (le-gpt--format-time-ago modified))))

         ((eq type 'buffer)
          ;; Compute buffer info lazily
          (let* ((buffer (assoc-default 'buffer metadata))
                 (size (when buffer (buffer-size buffer)))
                 (modified (when buffer (buffer-modified-p buffer)))
                 (mode (when buffer
                         (with-current-buffer buffer
                           major-mode)))
                 (file-path (when buffer (buffer-file-name buffer))))
            (format "%s%-8s %-8s %-12s%s%s"
                    (make-string padding ?\s)
                    "[Buffer]"
                    (if size (le-gpt--format-file-size size) "0B")
                    (if mode
                        (format "(%s)" (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
                      "")
                    (if modified " *mod*" "")
                    (if (and file-path (not (file-exists-p file-path))) " *del*" ""))))
         (t ""))))))

(defun le-gpt--get-context-group (completions)
  "Get group function for context COMPLETIONS with counts."
  (lambda (candidate transform)
    (if transform
        candidate
      (let* ((metadata (assoc-default candidate completions))
             (type (assoc-default 'type metadata))
             (file-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'file)) completions)))
             (buffer-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'buffer)) completions)))
             (history-count (length (seq-filter (lambda (c) (eq (assoc-default 'type (cdr c)) 'history)) completions))))
        (cond
         ((eq type 'history) (format "🕒 Recent Context (%d)" history-count))
         ((eq type 'file) (format "📄 Project Files (%d)" file-count))
         ((eq type 'buffer) (format "📋 Buffers (%d)" buffer-count))
         (t "Other"))))))

(defun le-gpt--context-collection-fn (completions)
  "Get collection function for context COMPLETIONS."
  (lambda (str pred flag)
    (cl-case flag
      (metadata
       `(metadata
         (annotation-function . ,(le-gpt--get-context-annotations completions))
         (group-function . ,(le-gpt--get-context-group completions))
         (display-sort-function . ,#'identity)))
      (t
       (all-completions str completions pred)))))

(defun le-gpt--read-multiple-context-items ()
  "Let user select multiple context items using proper completion metadata."
  (let* ((completions (le-gpt--create-context-completions))
         (available-choices (mapcar #'car completions))
         (choices available-choices)
         (selection nil)
         (done nil)
         (selected-items '()))

    (while (not done)
      (let ((selected-help (if selected-items
                               (concat "Currently selected:\n"
                                       (mapconcat
                                        (lambda (item)
                                          (let ((type (get-text-property 0 'context-type item)))
                                            (format "  %s %s"
                                                    (if (eq type 'file) "📄" "📋")
                                                    item)))
                                        selected-items "\n")
                                       "\n\n")
                             "No items selected yet\n\n")))

        (setq selection

              (completing-read
               (format "%sSelect context item (empty input when done) [%d selected]: "
                       selected-help
                       (length selected-items))
               (le-gpt--context-collection-fn
                ;; Filter completions to only show remaining choices
                (seq-filter (lambda (comp) (member (car comp) choices)) completions))
               nil nil nil nil ""))

        (if (string-empty-p selection)
            (setq done t)
          ;; Find the original completion item to restore text properties
          (let ((original-item (car (seq-find (lambda (comp)
                                                (string= (car comp) selection))
                                              completions))))
            (when original-item

              (if (eq (get-text-property 0 'context-type original-item) 'history)
                  (progn
                    ;; Use the stored items from history
                    (setq selected-items (get-text-property 0 'context-items original-item))
                    (setq done t))
                (push original-item selected-items)
                (setq choices (delete selection choices))))))))

    (nreverse selected-items)))

(defun le-gpt--get-all-context (use-interactive)
  "Get combined context string from pending and optionally interactive selection.
If USE-INTERACTIVE is non-nil, also prompt for additional context.
Pending context is cleared after retrieval."
  (let* ((pending-count (le-gpt--pending-context-count))
         (pending (le-gpt--pending-context-get-formatted))
         (selected (when use-interactive (le-gpt--get-context)))
         (result nil))
    ;; Combine pending and selected context
    (setq result
          (cond
           ((and pending selected) (concat pending "\n\n" selected))
           (pending pending)
           (selected selected)
           (t nil)))
    ;; Clear pending context after use
    (when pending
      (le-gpt--pending-context-clear)
      (message "Le GPT: Included %d pending context item%s (now cleared)"
               pending-count
               (if (= pending-count 1) "" "s")))
    result))

(provide 'le-gpt-context)
;;; le-gpt-context.el ends here
