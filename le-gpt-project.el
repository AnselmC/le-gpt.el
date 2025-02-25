;;; le-gpt-project.el --- Project context functionality for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;

;;; Code:

(require 'le-gpt-core)
(require 'project)

(defvar le-gpt--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar le-gpt--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

(defun le-gpt-clear-selected-context-files ()
  "Clear the list of project files used as context."
  (interactive)
  (setq le-gpt--selected-context-files nil)
  (message "Project file context cleared."))

(defun le-gpt-select-project-files-for-context ()
  "Prompt user to select files from project to use as context."
  (let* ((relative-files (le-gpt--get-project-files))
         ;; Remove already selected files from choices
         (available-files (seq-difference relative-files le-gpt--selected-context-files)))
    (when-let ((new-selections (le-gpt--read-multiple-files available-files)))
      (setq le-gpt--selected-context-files
            (append le-gpt--selected-context-files new-selections))
      (message "Added %d files. Total files selected: %d"
               (length new-selections)
               (length le-gpt--selected-context-files)))))

(defun le-gpt-deselect-project-files-for-context ()
  "Remove multiple files from the current project context."
  (if (null le-gpt--selected-context-files)
      (message "No files currently selected")
    (let ((to-remove (le-gpt--read-multiple-files-to-remove le-gpt--selected-context-files)))
      (setq le-gpt--selected-context-files
            (seq-difference le-gpt--selected-context-files to-remove))
      (message "Removed %d files. Remaining files: %d"
               (length to-remove)
               (length le-gpt--selected-context-files)))))

(defun le-gpt--get-project-context (select-context-files)
  "Get the formatted project context string based on the selected files.
If SELECT-CONTEXT-FILES is non-nil, prompt for new file selection.
Else use globally selected context files."
  (let ((selected-context-files (le-gpt--read-or-get-selected-context-files select-context-files)))
    (when selected-context-files
      (format le-gpt--project-context-format
              (mapconcat #'identity selected-context-files "\n")
              (le-gpt--get-selected-files-contents selected-context-files)))))

(defun le-gpt--read-or-get-selected-context-files (temp-context-files)
  "Get selected context files or prompt for new ones.
If TEMP-CONTEXT-FILES is non-nil, prompt for new files."
  (if temp-context-files (le-gpt--read-multiple-files
                          (le-gpt--get-project-files))
    le-gpt--selected-context-files))

(defun le-gpt--get-selected-files-contents (selected-context-files)
  "Get contents of SELECTED-CONTEXT-FILES as a formatted string."
  (let ((file-contents "")
        (project-root (project-root (project-current))))
    (dolist (file selected-context-files)
      (condition-case err
          (setq file-contents
                (concat file-contents
                        (format "\nFile: %s\n```\n%s\n```\n"
                                file
                                (with-temp-buffer
                                  (insert-file-contents
                                   (expand-file-name file project-root))
                                  (let ((content (buffer-string)))
                                    content)))))
        (error
         (message "Error reading file %s: %s" file (error-message-string err)))))
    file-contents))

(defun le-gpt--read-multiple-files (files)
  "Let user select multiple FILES using completion.
Shows currently selected files.  Empty input finishes selection."
  (let ((choices files)
        (selection nil)
        (done nil)
        (selected-files le-gpt--selected-context-files))
    (while (not done)
      (let ((selected-help (if selected-files
                               (concat "Currently selected:\n"
                                       (mapconcat (lambda (f) (concat "  - " f))
                                                  selected-files "\n")
                                       "\n\n")
                             "No files selected yet\n\n")))
        (setq selection
              (completing-read
               (format "%sSelect file (empty input when done) [%d selected]: "
                       selected-help
                       (length selected-files))
               choices nil nil nil nil ""))

        (if (string-empty-p selection)
            (setq done t)
          (push selection selected-files)
          (setq choices (delete selection choices)))))
    (nreverse selected-files)))

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el."
  (let ((current-project (project-current)))
    (if current-project
        (mapcar (lambda (f)
                  (file-relative-name f (project-root (project-current))))
                (project-files current-project))
      (error "Not in any project recognized by project.el"))))

(defun le-gpt--read-multiple-files-to-remove (files)
  "Let user select multiple FILES to remove using completion.
Shows files selected for removal.  Empty input finishes selection."
  (let ((choices files)
        (selection nil)
        (done nil)
        (files-to-remove '()))
    (while (not done)
      (let ((remove-help (if files-to-remove
                             (concat "Selected for removal:\n"
                                     (mapconcat (lambda (f) (concat "  - " f))
                                                files-to-remove "\n")
                                     "\n\n")
                           "No files selected for removal yet\n\n")))
        (setq selection
              (completing-read
               (format "%sSelect file to remove (empty to finish) [%d to remove]: "
                       remove-help
                       (length files-to-remove))
               choices nil nil))

        (if (string-empty-p selection)
            (setq done t)
          (push selection files-to-remove)
          (setq choices (delete selection choices)))))
    (nreverse files-to-remove)))

(provide 'le-gpt-project)
;;; le-gpt-project.el ends here
