;;; le-gpt-context-files.el --- File context handling for le-gpt -*- lexical-binding: t; -*-

;; License: MIT
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; File and project context handling functionality.

;;; Code:

(require 'project)
(require 'le-gpt-context-lang)
(require 'le-gpt-context-utils)

(defcustom le-gpt-max-file-size (* 1024 1024) ; 1MB
  "Maximum file size in bytes to include in context."
  :type 'integer
  :group 'le-gpt)

(defcustom le-gpt-max-total-context-size (* 10 1024 1024) ; 10MB
  "Maximum total size of all context files combined."
  :type 'integer
  :group 'le-gpt)

(defcustom le-gpt-file-preview-lines 1000
  "Number of lines to show for large files (0 = show all)."
  :type 'integer
  :group 'le-gpt)

;; Cache variables
(defvar le-gpt--file-cache (make-hash-table :test 'equal)
  "Cache for file metadata and contents.")

(defvar le-gpt--project-files-cache nil
  "Cache for project files list.")

(defvar le-gpt--cache-project-root nil
  "Project root for cached files.")

(defvar le-gpt--selected-context-files nil
  "List of project files whose content should be included as context.")

(defvar le-gpt--project-context-format
  "Project Context Files:\n%s\n\nFile Contents:\n%s\n\n"
  "Format string for including project context in prompts.
First %s is replaced with the list of files, second with their contents.")

;; Binary file extensions (moved to defconst for efficiency)
(defconst le-gpt--binary-extensions
  '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp" "ico"
    "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx" "odt" "ods" "odp"
    "zip" "tar" "gz" "bz2" "7z" "rar" "xz" "lz" "lzma"
    "mp3" "mp4" "avi" "mov" "mkv" "wav" "flac" "ogg" "m4a"
    "exe" "dll" "so" "dylib" "bin" "deb" "rpm" "dmg" "msi"
    "class" "jar" "war" "ear" "pyc" "pyo" "o" "obj"
    "ttf" "otf" "woff" "woff2" "eot"
    "sqlite" "db" "mdb")
  "File extensions to exclude as binary files.")

(defconst le-gpt--text-extensions
  '("txt" "md" "org" "rst" "tex" "log" "cfg" "conf" "ini"
    "json" "xml" "yaml" "yml" "toml" "csv" "tsv"
    "html" "htm" "css" "js" "ts" "jsx" "tsx" "vue" "svelte"
    "py" "rb" "php" "java" "c" "cpp" "h" "hpp" "cs" "go" "rs"
    "el" "lisp" "clj" "hs" "ml" "scala" "kt" "swift" "dart"
    "sh" "bash" "zsh" "fish" "ps1" "bat" "cmd"
    "sql" "r" "m" "pl" "lua" "vim" "dockerfile" "makefile"
    "gitignore" "gitattributes" "editorconfig" "eslintrc"
    "prettierrc" "babelrc" "npmrc" "yarnrc" "tf" "tfvars" "hcl"
    "proto" "graphql" "gql" "mdx" "scss" "sass" "less" "ex" "exs" "jl"
    "mod" "sum" "nix" "helm" "k8s" "kube" "dockerfile")
  "File extensions known to be text files.")


(defun le-gpt--clear-cache ()
  "Clear file cache."
  (interactive)
  (clrhash le-gpt--file-cache)
  (setq le-gpt--project-files-cache nil
        le-gpt--cache-project-root nil)
  (message "le-gpt cache cleared"))

(defun le-gpt--get-file-info-cached (file-path)
  "Get cached file info or compute it. Returns (size . mtime)."
  (let* ((full-path (expand-file-name file-path))
         (cached (gethash full-path le-gpt--file-cache)))
    (if cached
        cached
      (condition-case nil
          (let* ((attrs (file-attributes full-path))
                 (size (file-attribute-size attrs))
                 (mtime (file-attribute-modification-time attrs))
                 (info (cons size mtime)))
            (puthash full-path info le-gpt--file-cache)
            info)
        (error (cons 0 nil))))))

(defun le-gpt--get-project-files ()
  "Get list of files in current project using project.el with caching."
  (let* ((current-project (project-current))
         (project-root (and current-project (project-root current-project))))
    (unless current-project
      (error "Not in any project recognized by project.el"))

    ;; Check if cache is valid
    (if (and le-gpt--project-files-cache
             (equal project-root le-gpt--cache-project-root))
        (progn
          (when le-gpt-debug-timing
            (message "[le-gpt timing] Using cached project files (%d files)"
                     (length le-gpt--project-files-cache)))
          le-gpt--project-files-cache)
      ;; Rebuild cache
      (when le-gpt-debug-timing
        (message "[le-gpt timing] Building project files cache..."))
      (condition-case err
          (let* ((all-files
                  (project-files current-project))
                 (relative-files
                  (mapcar (lambda (f)
                            (file-relative-name f project-root))
                          all-files)))
            (when le-gpt-debug-timing
              (message "[le-gpt timing] Found %d total files, filtering..."
                       (length all-files)))
            (let ((files
                   (seq-filter #'le-gpt--is-text-file-p relative-files)))
              (setq le-gpt--project-files-cache files
                    le-gpt--cache-project-root project-root)
              (when le-gpt-debug-timing
                (message "[le-gpt timing] Cached %d text files" (length files)))
              files))
        (error
         ;; Clear cache on error
         (setq le-gpt--project-files-cache nil
               le-gpt--cache-project-root nil)
         (signal (car err) (cdr err)))))))

(defun le-gpt--is-text-file-p (file-path)
  "Return t if FILE-PATH is likely a text file (optimized version)."
  (let* ((project-root (project-root (project-current)))
         (full-path (if (file-name-absolute-p file-path)
                        file-path
                      (expand-file-name file-path project-root))))

    ;; Basic checks first (before getting file info)
    (and (file-readable-p full-path)
         (not (file-directory-p full-path))

         ;; Then get cached info
         (let* ((file-info (le-gpt--get-file-info-cached full-path))
                (size (car file-info))
                (ext (downcase (or (file-name-extension full-path) ""))))

           (and
            ;; Size checks
            (> size 0)
            (< size le-gpt-max-file-size)
            ;; Extension-based filtering
            (not (member ext le-gpt--binary-extensions))
            ;; For known text extensions, skip content check
            (or (member ext le-gpt--text-extensions)
                ;; For unknown extensions, do content check
                (and (string-empty-p ext)
                     (le-gpt--file-appears-textual-p full-path))))))))

(defun le-gpt--file-appears-textual-p (file-path)
  "Check if FILE-PATH appears to contain text (optimized version)."
  (condition-case nil
      (with-temp-buffer
        ;; Read smaller sample for faster check
        (insert-file-contents file-path nil 0 512)
        (let ((content (buffer-string)))
          (and (> (length content) 0)
               ;; More efficient character checking
               (let ((non-printable 0)
                     (total (length content)))
                 (dotimes (i total)
                   (let ((c (aref content i)))
                     (when (and (< c 32) (not (memq c '(9 10 13)))) ; tab, newline, carriage return
                       (setq non-printable (1+ non-printable)))))
                 (< (/ non-printable (float total)) 0.1))))) ; Stricter threshold
    (error nil)))

(defun le-gpt--get-selected-files-contents (selected-context-files)
  "Get contents of SELECTED-CONTEXT-FILES with size limits and progress."
  (let ((file-contents "")
        (project-root (project-root (project-current)))
        (total-size 0)
        (processed 0)
        (total-files (length selected-context-files)))

    (message "Loading context files...")

    (dolist (file selected-context-files)
      (setq processed (1+ processed))
      (when (and (zerop (% processed 10))
                 (not le-gpt-debug-timing))
        (message "Processing file %d/%d..." processed total-files))

      (condition-case err
          (let* ((full-path (expand-file-name file project-root))
                 (file-info (le-gpt--get-file-info-cached full-path))
                 (file-size (car file-info)))

            ;; Check total size limit
            (when (> (+ total-size file-size) le-gpt-max-total-context-size)
              (message "Reached total context size limit, stopping at %d files" processed)
              (cl-return))

            (let* ((language
                    (format "get-language-for-file (%s)" file)
                    (le-gpt--get-language-for-file full-path))
                   (content
                    (format "read-file-content (%s)" file)
                    (le-gpt--get-file-content-limited full-path file-size)))
              (setq file-contents
                    (concat file-contents
                            (format "\nFile: %s (%s)\n```%s\n%s\n```\n"
                                    file
                                    (le-gpt--format-file-size file-size)
                                    language
                                    content)))
              (setq total-size (+ total-size file-size))))
        (error
         (message "Error reading file %s: %s" file (error-message-string err)))))

    (message "Loaded %d files (%s total)"
             processed
             (le-gpt--format-file-size total-size))
    file-contents))

(defun le-gpt--get-file-content-limited (file-path file-size)
  "Get file content with size/line limits."
  (with-temp-buffer
    (if (and (> le-gpt-file-preview-lines 0)
             (> file-size (* 50 1024))) ; 50KB threshold
        ;; For large files, read limited lines
        (progn
          (insert-file-contents file-path)
          (goto-char (point-min))
          (let ((line-count 0)
                (content ""))
            (while (and (< line-count le-gpt-file-preview-lines)
                        (not (eobp)))
              (setq content (concat content (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position)) "\n"))
              (setq line-count (1+ line-count))
              (forward-line 1))
            (when (not (eobp))
              (setq content (concat content
                                    (format "\n... [truncated after %d lines] ..."
                                            le-gpt-file-preview-lines))))
            content))
      ;; For small files, read everything
      (insert-file-contents file-path)
      (buffer-string))))

(defun le-gpt--format-file-size (size)
  "Format file SIZE in human-readable format."
  (cond
   ((< size 1024) (format "%d B" size))
   ((< size (* 1024 1024)) (format "%.1f KB" (/ size 1024.0)))
   (t (format "%.1f MB" (/ size 1024.0 1024.0)))))

;; Keep existing utility functions
(defun le-gpt--get-file-size (file-path)
  "Get size of FILE-PATH in bytes."
  (car (le-gpt--get-file-info-cached file-path)))

(defun le-gpt--get-file-modified-time (file-path)
  "Get modification time of FILE-PATH."
  (cdr (le-gpt--get-file-info-cached file-path)))

(defun le-gpt--count-file-lines (file-path)
  "Count lines in FILE-PATH efficiently."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (count-lines (point-min) (point-max)))
    (error 0)))

(provide 'le-gpt-context-files)
;;; le-gpt-context-files.el ends here
