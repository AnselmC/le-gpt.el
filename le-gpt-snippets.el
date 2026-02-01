;;; le-gpt-snippets.el --- System prompt snippets for le-gpt.el -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Manage reusable system prompt snippets that can be toggled on/off
;; and combined when making GPT requests.

;;; Code:

(require 'tabulated-list)

;;; Customization

(defcustom le-gpt-snippets-file
  (expand-file-name "le-gpt-snippets.el" user-emacs-directory)
  "File to store system prompt snippets."
  :type 'file
  :group 'le-gpt)

;;; Data structure
;; Each snippet is a plist:
;; (:name "code-reviewer"
;;  :content "You are an expert code reviewer..."
;;  :enabled t
;;  :order 10)

(defvar le-gpt-snippets--list nil
  "List of system prompt snippets.")

;;; Persistence

(defun le-gpt-snippets--save ()
  "Save snippets to `le-gpt-snippets-file'."
  (with-temp-file le-gpt-snippets-file
    (insert ";; le-gpt snippets - auto-generated, do not edit manually\n")
    (prin1 le-gpt-snippets--list (current-buffer))
    (insert "\n")))

(defun le-gpt-snippets--load ()
  "Load snippets from `le-gpt-snippets-file'."
  (when (file-exists-p le-gpt-snippets-file)
    (with-temp-buffer
      (insert-file-contents le-gpt-snippets-file)
      (goto-char (point-min))
      (forward-line 1) ; skip comment
      (setq le-gpt-snippets--list (read (current-buffer))))))

;; Load snippets on startup
(le-gpt-snippets--load)

;;; Core functions

(defun le-gpt-snippets--get (name)
  "Get snippet by NAME."
  (seq-find (lambda (s) (string= (plist-get s :name) name))
            le-gpt-snippets--list))

(defun le-gpt-snippets--get-combined ()
  "Return concatenated content of all enabled snippets, ordered by :order."
  (let* ((enabled (seq-filter (lambda (s) (plist-get s :enabled))
                              le-gpt-snippets--list))
         (sorted (seq-sort (lambda (a b)
                             (< (or (plist-get a :order) 100)
                                (or (plist-get b :order) 100)))
                           enabled))
         (contents (mapcar (lambda (s) (plist-get s :content)) sorted)))
    (when contents
      (mapconcat #'identity contents "\n\n"))))

;;; Public commands

;;;###autoload
(defun le-gpt-snippet-add (name content &optional order)
  "Add a new snippet with NAME and CONTENT.
Optional ORDER determines priority (lower = higher priority)."
  (interactive
   (list (read-string "Snippet name: ")
         (read-string "Snippet content: ")
         (read-number "Order (lower = higher priority): " 50)))
  (when (le-gpt-snippets--get name)
    (user-error "Snippet '%s' already exists" name))
  (push (list :name name :content content :enabled nil :order (or order 50))
        le-gpt-snippets--list)
  (le-gpt-snippets--save)
  (message "Added snippet '%s'" name)
  (when (derived-mode-p 'le-gpt-snippets-list-mode)
    (le-gpt-snippets--refresh)))

;;;###autoload
(defun le-gpt-snippet-remove (name)
  "Remove snippet by NAME."
  (interactive
   (list (completing-read "Remove snippet: "
                          (mapcar (lambda (s) (plist-get s :name))
                                  le-gpt-snippets--list)
                          nil t)))
  (setq le-gpt-snippets--list
        (seq-remove (lambda (s) (string= (plist-get s :name) name))
                    le-gpt-snippets--list))
  (le-gpt-snippets--save)
  (message "Removed snippet '%s'" name)
  (when (derived-mode-p 'le-gpt-snippets-list-mode)
    (le-gpt-snippets--refresh)))

;;;###autoload
(defun le-gpt-snippet-edit (name)
  "Edit the content of snippet NAME."
  (interactive
   (list (completing-read "Edit snippet: "
                          (mapcar (lambda (s) (plist-get s :name))
                                  le-gpt-snippets--list)
                          nil t)))
  (let ((snippet (le-gpt-snippets--get name)))
    (unless snippet
      (user-error "Snippet '%s' not found" name))
    (let ((new-content (read-string "New content: " (plist-get snippet :content))))
      (plist-put snippet :content new-content)
      (le-gpt-snippets--save)
      (message "Updated snippet '%s'" name)
      (when (derived-mode-p 'le-gpt-snippets-list-mode)
        (le-gpt-snippets--refresh)))))

;;;###autoload
(defun le-gpt-snippet-toggle (name)
  "Toggle the enabled state of snippet NAME."
  (interactive
   (list (completing-read "Toggle snippet: "
                          (mapcar (lambda (s) (plist-get s :name))
                                  le-gpt-snippets--list)
                          nil t)))
  (let ((snippet (le-gpt-snippets--get name)))
    (unless snippet
      (user-error "Snippet '%s' not found" name))
    (plist-put snippet :enabled (not (plist-get snippet :enabled)))
    (le-gpt-snippets--save)
    (message "Snippet '%s' is now %s"
             name (if (plist-get snippet :enabled) "enabled" "disabled"))
    (when (derived-mode-p 'le-gpt-snippets-list-mode)
      (le-gpt-snippets--refresh))))

;;; Tabulated list UI

(defun le-gpt-snippets--refresh ()
  "Refresh the snippets list buffer."
  (setq tabulated-list-entries
        (mapcar (lambda (s)
                  (list (plist-get s :name)
                        (vector
                         (if (plist-get s :enabled) "*" " ")
                         (number-to-string (or (plist-get s :order) 50))
                         (plist-get s :name)
                         (truncate-string-to-width
                          (replace-regexp-in-string "\n" " " (plist-get s :content))
                          50 nil nil t))))
                le-gpt-snippets--list))
  (tabulated-list-print t))

(defun le-gpt-snippets-list--toggle ()
  "Toggle the snippet at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when name
      (le-gpt-snippet-toggle name))))

(defun le-gpt-snippets-list--edit ()
  "Edit the snippet at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when name
      (le-gpt-snippet-edit name))))

(defun le-gpt-snippets-list--add ()
  "Add a new snippet."
  (interactive)
  (call-interactively #'le-gpt-snippet-add))

(defun le-gpt-snippets-list--delete ()
  "Delete the snippet at point."
  (interactive)
  (let ((name (tabulated-list-get-id)))
    (when (and name (yes-or-no-p (format "Delete snippet '%s'? " name)))
      (le-gpt-snippet-remove name))))

(defun le-gpt-snippets-list--increase-order ()
  "Increase the order (lower priority) of snippet at point."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (snippet (le-gpt-snippets--get name)))
    (when snippet
      (plist-put snippet :order (+ (or (plist-get snippet :order) 50) 10))
      (le-gpt-snippets--save)
      (le-gpt-snippets--refresh))))

(defun le-gpt-snippets-list--decrease-order ()
  "Decrease the order (higher priority) of snippet at point."
  (interactive)
  (let* ((name (tabulated-list-get-id))
         (snippet (le-gpt-snippets--get name)))
    (when snippet
      (plist-put snippet :order (max 0 (- (or (plist-get snippet :order) 50) 10)))
      (le-gpt-snippets--save)
      (le-gpt-snippets--refresh))))

(defvar le-gpt-snippets-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'le-gpt-snippets-list--edit)
    (define-key map (kbd "a") #'le-gpt-snippets-list--add)
    (define-key map (kbd "d") #'le-gpt-snippets-list--delete)
    (define-key map (kbd "t") #'le-gpt-snippets-list--toggle)
    (define-key map (kbd "+") #'le-gpt-snippets-list--decrease-order)
    (define-key map (kbd "-") #'le-gpt-snippets-list--increase-order)
    (define-key map (kbd "g") #'le-gpt-snippets--refresh)
    map)
  "Keymap for `le-gpt-snippets-list-mode'.")

(define-derived-mode le-gpt-snippets-list-mode tabulated-list-mode "Le GPT Snippets"
  "Major mode for managing le-gpt system prompt snippets."
  (setq tabulated-list-format
        [("E" 1 t)
         ("Order" 5 t :right-align t)
         ("Name" 20 t)
         ("Content" 50 t)])
  (setq tabulated-list-sort-key '("Order" . nil))
  (tabulated-list-init-header))

;;;###autoload
(defun le-gpt-snippet-list ()
  "Open buffer to manage system prompt snippets."
  (interactive)
  (let ((buffer (get-buffer-create "*Le GPT Snippets*")))
    (with-current-buffer buffer
      (le-gpt-snippets-list-mode)
      (le-gpt-snippets--refresh))
    (switch-to-buffer buffer)))

(provide 'le-gpt-snippets)

;;; le-gpt-snippets.el ends here
