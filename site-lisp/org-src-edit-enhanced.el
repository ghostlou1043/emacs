;;; org-src-edit-enhanced.el --- Enhanced multi-block source code editing for Org mode -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org, convenience, src-block
;; URL: https://github.com/yourusername/org-src-edit-enhanced

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; org-src-edit-enhanced provides enhanced source code block editing capabilities for Org mode.
;; 
;; Main features:
;; - Open source blocks in a dedicated buffer with proper language mode
;; - Support for multi-block editing with tangle and session awareness
;; - Real file support for LSP and other language server integration
;; - Automatic read-only protection for non-editable blocks
;; - Seamless integration with Org files

;;; Code:

(require 'org)
(require 'org-element)
(require 'ob-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 1: CUSTOMIZATION VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup org-src-edit-enhanced nil
  "Enhanced source code block editing for Org mode."
  :group 'org
  :prefix "org-src-edit-")

(defcustom org-src-edit-root-dir "~/org-src/"
  "Root directory for temporary project files.

When editing source blocks without explicit tangling, files are created
under this directory in subdirectories named after the block's language,
session, and file identifier."
  :type 'directory
  :group 'org-src-edit-enhanced)

(defcustom org-src-edit-entry-filename "main"
  "Base filename for entry points in temporary projects.

For example, if set to 'main', Python blocks would create 'main.py'."
  :type 'string
  :group 'org-src-edit-enhanced)

(defcustom org-src-edit-language-extensions
  '(:python "py"
            :shell "sh"
            :bash "sh"
            :elisp "el"
            :javascript "js"
            :typescript "ts"
            :java "java"
            :cpp "cpp"
            :c "c"
            :rust "rs"
            :go "go"
            :ruby "rb"
            :lua "lua"
            :perl "pl"
            :php "php"
            :sql "sql"
            :html "html"
            :css "css"
            :json "json"
            :yaml "yaml"
            :xml "xml"
            :clojure "clj"
            :scheme "scm"
            :haskell "hs")
  "Mapping from language names to file extensions.

Each element is a keyword (language name) followed by a string (extension).
Extensions should not include the leading dot."
  :type 'plist
  :group 'org-src-edit-enhanced)

(defcustom org-src-edit-comment-styles
  '(:python "#"
            :shell "#"
            :bash "#"
            :elisp ";"
            :javascript "//"
            :typescript "//"
            :java "//"
            :cpp "//"
            :c "//"
            :rust "//"
            :go "//"
            :ruby "#"
            :lua "--"
            :perl "#"
            :php "//"
            :sql "--"
            :html "<!--"
            :css "/*"
            :json "//"
            :yaml "#"
            :xml "<!--"
            :clojure ";"
            :scheme ";"
            :haskell "--")
  "Mapping from language names to comment characters/strings.

Each element is a keyword (language name) followed by a string (comment marker).
For languages with multi-character comments (like HTML), use the opening marker."
  :type 'plist
  :group 'org-src-edit-enhanced)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 2: INTERNAL STATE & BUFFER-LOCAL VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local org-src-edit--session-info nil
  "Buffer-local storage for editing session information.

This is a plist containing:
  :org-buffer - The original Org file buffer
  :org-marker - Marker pointing to the src-block in the Org file
  :language - The programming language of the block
  :comment-char - The comment character for this language
  :editable-begin - Marker for the start of the editable region
  :editable-end - Marker for the end of the editable region
  :related-blocks - List of all related blocks info (for multi-block editing)
  :target-file - Path to the file being edited

This variable is only set in editing buffers created by org-src-edit-enhanced.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 3: UTILITY FUNCTIONS (Simple, no dependencies)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--get-extension (language)
  "Get file extension for the given LANGUAGE.

LANGUAGE should be a string (e.g., 'python', 'javascript').
Returns the extension without the leading dot (e.g., 'py', 'js').
Returns 'txt' if the language is not recognized."
  (let* ((lang-keyword (intern (concat ":" language)))
         (ext (plist-get org-src-edit-language-extensions lang-keyword)))
    (or ext "txt")))

(defun org-src-edit--get-comment-char (language)
  "Get the comment character(s) for the given LANGUAGE.

LANGUAGE should be a string.
Returns a string suitable for prefixing comments.
Defaults to '#' if language is unknown."
  (let* ((lang-keyword (intern (concat ":" language)))
         (comment (plist-get org-src-edit-comment-styles lang-keyword)))
    (or comment "#")))

(defun org-src-edit--file-identifier (filepath)
  "Generate a short, unique identifier for FILEPATH.

Uses the first 8 characters of the MD5 hash of FILEPATH.
This is used to create unique directory names for temporary projects."
  (substring (md5 filepath) 0 8))

(defun org-src-edit--make-begin-marker (block-number status line-number comment-char)
  "Create a begin marker line for a code block.

BLOCK-NUMBER is the block's index (integer or string 'Block' for single).
STATUS is 'EDITABLE' or 'Read Only'.
LINE-NUMBER is the line number in the Org file.
COMMENT-CHAR is the single-line comment marker for this language.

Returns a formatted string like: '# [Block 1 - EDITABLE - Line 45]'"
  (format "%s [%s - %s - Line %s]\n" comment-char block-number status line-number))

(defun org-src-edit--make-end-marker (block-number comment-char)
  "Create an end marker line for a code block.

BLOCK-NUMBER is the block's index (integer or string 'Block' for single).
COMMENT-CHAR is the single-line comment marker.

Returns a formatted string like: '# [End Block 1]'"
  (format "%s [End %s]\n" comment-char block-number))

(defun org-src-edit--expand-tilde (path)
  "Expand ~ and ~user in PATH to absolute directory.

This is a wrapper around expand-file-name for convenience."
  (expand-file-name path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 4: FILE MANAGEMENT FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--get-target-file (language session tangle effective-dir org-file)
  "Determine the target file path for editing.

LANGUAGE is the programming language (e.g., 'python').
SESSION is the session name or nil.
TANGLE is the tangle filename or nil.
EFFECTIVE-DIR is the effective :dir or nil.
ORG-FILE is the absolute path to the Org file.

Returns the absolute path to the target file.

Logic:
  1. If TANGLE is provided, use expand-file-name(tangle, effective-dir)
  2. Otherwise:
     - Compute file identifier from ORG-FILE
     - If SESSION: folder = '{identifier}-{language}-{session}'
     - If no SESSION: folder = '{identifier}-{language}'
     - Create folder under org-src-edit-root-dir
     - Return {folder}/main.{extension}"
  (if tangle
      ;; Case 1: Explicit tangle
      (expand-file-name tangle (or effective-dir (user-homedir-pathname)))
    ;; Case 2: Auto-generated file
    (let* ((identifier (org-src-edit--file-identifier org-file))
           (ext (org-src-edit--get-extension language))
           (folder-name (if session
                            (format "%s-%s-%s" identifier language session)
                          (format "%s-%s" identifier language)))
           (root (org-src-edit--expand-tilde org-src-edit-root-dir))
           (project-dir (concat (file-name-as-directory root) folder-name)))
      (concat (file-name-as-directory project-dir)
              org-src-edit-entry-filename "." ext))))

(defun org-src-edit--ensure-directory (filepath)
  "Ensure the directory for FILEPATH exists.

Creates parent directories if needed.
FILEPATH should be an absolute file path."
  (let ((dir (file-name-directory filepath)))
    (when dir
      (make-directory dir t))))

(defun org-src-edit--ensure-file-exists (filepath)
  "Ensure FILEPATH exists as a file.

If the file doesn't exist, creates it (and parent directories).
If the file already exists, does nothing."
  (org-src-edit--ensure-directory filepath)
  (unless (file-exists-p filepath)
    (write-region "" nil filepath nil 'silent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 5: BLOCK INFORMATION EXTRACTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--get-block-info-at-point ()
  "Extract comprehensive information about the src-block at point.

Returns a plist with keys:
  :language - Programming language (string)
  :body - Source code content (string)
  :session - Session name from :session header arg or nil
  :tangle - Tangle filename from :tangle header arg or nil
  :dir - Directory from :dir header arg or nil
  :begin - Buffer position where block starts
  :end - Buffer position where block ends
  :line-number - Line number in buffer where block starts
  :name - Block name or nil
  :element - The org-element object itself

Returns nil if point is not on a src-block."
  (let ((element (org-element-at-point)))
    (when-let* ((lang-name (org-element-property :language element))
                (block-body (org-element-property :value element))
                (info (org-babel-get-src-block-info 'no-eval element)))
      ;; info structure: (language body arguments switches name start coderef)
      (pcase-let ((`(,language ,body ,arguments ,_switches ,name ,_start ,_coderef) info))
        (list
         :language (downcase language)
         :body body
         :session (plist-get arguments :session)
         :tangle (plist-get arguments :tangle)
         :dir (plist-get arguments :dir)
         :begin (org-element-property :begin element)
         :end (org-element-property :end element)
         :line-number (line-number-at-pos (org-element-property :begin element))
         :name name
         :element element)))))

(defun org-src-edit--get-effective-dir (dir-from-block org-file)
  "Determine the effective :dir for a block.

DIR-FROM-BLOCK is the :dir from the block's header args or nil.
ORG-FILE is the absolute path to the Org file.

Priority:
  1. If DIR-FROM-BLOCK is provided, use it
  2. Otherwise, check file-level #+PROPERTY: header-args :dir
  3. Otherwise, return nil

Returns an absolute directory path or nil."
  (or dir-from-block
      ;; Try to get file-level property
      (when-let* ((file-dir (org-entry-get nil "header-args")))
        ;; Parse the :dir from header-args string if present
        ;; For now, we'll just return nil for file-level (simplified)
        nil)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 6: RELATED BLOCKS COLLECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--collect-related-blocks (current-block-info org-buffer)
  "Collect all related blocks that should be edited together.

CURRENT-BLOCK-INFO is the plist from org-src-edit--get-block-info-at-point.
ORG-BUFFER is the Org file buffer.

Returns a list of block-info plists, ordered by position in the file.

Grouping logic:
  1. If TANGLE is set: collect all blocks with same tangle + effective dir
  2. If SESSION is set (no tangle): collect all blocks with same session + language
  3. Otherwise: return only the current block"
  (let* ((language (plist-get current-block-info :language))
         (session (plist-get current-block-info :session))
         (tangle (plist-get current-block-info :tangle))
         (effective-dir (plist-get current-block-info :dir))
         related-blocks)
    
    (with-current-buffer org-buffer
      ;; Parse the entire buffer to get all src-blocks
      (let ((tree (org-element-parse-buffer 'element)))
        (org-element-map tree 'src-block
          (lambda (block)
            (let* ((block-lang (downcase (org-element-property :language block)))
                   (block-info (org-babel-get-src-block-info 'no-eval block))
                   (block-session (plist-get (nth 2 block-info) :session))
                   (block-tangle (plist-get (nth 2 block-info) :tangle))
                   (block-dir (plist-get (nth 2 block-info) :dir)))
              
              ;; Check if this block should be included
              (when (cond
                     ;; Case 1: Matching by tangle
                     (tangle
                      (and (string-equal block-tangle tangle)
                           (string-equal (or block-dir "") (or effective-dir ""))))
                     ;; Case 2: Matching by session
                     (session
                      (and (string-equal block-lang language)
                           (string-equal (or block-session "") (or session ""))))
                     ;; Case 3: Only current block
                     (t (eq block (plist-get current-block-info :element))))
                
                ;; Create block info for this block
                (push (list
                       :element block
                       :language block-lang
                       :body (org-element-property :value block)
                       :begin (org-element-property :begin block)
                       :end (org-element-property :end block)
                       :line-number (line-number-at-pos (org-element-property :begin block)))
                      related-blocks)))))))
    
    ;; Sort by position (begin marker)
    (sort related-blocks (lambda (a b)
                           (< (plist-get a :begin) (plist-get b :begin))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 7 (修订): BUILD EDIT CONTENT - 返回 Markers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--build-edit-content (related-blocks current-element comment-char)
  "Build the content for the editing buffer.

RELATED-BLOCKS is a list of block-info plists from org-src-edit--collect-related-blocks.
CURRENT-ELEMENT is the org-element of the current (editable) block.
COMMENT-CHAR is the comment character for this language.

Returns a list (CONTENT EDITABLE-BEGIN EDITABLE-END) where:
  CONTENT is the complete buffer content string
  EDITABLE-BEGIN is the position (integer) where editable region starts
  EDITABLE-END is the position (integer) where editable region ends"
  (let ((content "")
        (block-counter 0)
        editable-start
        editable-end)
    
    (dolist (block-info related-blocks)
      (setq block-counter (+ block-counter 1))
      (let* ((is-current (eq (plist-get block-info :element) current-element))
             (status (if is-current "EDITABLE" "Read Only"))
             (line-number (plist-get block-info :line-number))
             (body (plist-get block-info :body))
             (begin-marker (org-src-edit--make-begin-marker
                            (if (= (length related-blocks) 1) "Block" block-counter)
                            status
                            line-number
                            comment-char)))
        
        ;; Track editable region start (after begin marker)
        (when is-current
          (setq editable-start (length content)))
        
        ;; Add begin marker
        (setq content (concat content begin-marker))
        
        ;; Add body (ensure it ends with newline)
        (unless (string-suffix-p "\n" body)
          (setq body (concat body "\n")))
        (setq content (concat content body))
        
        ;; Track editable region end (before end marker)
        (when is-current
          (setq editable-end (length content)))
        
        ;; Add end marker
        (let ((end-marker (org-src-edit--make-end-marker
                           (if (= (length related-blocks) 1) "Block" block-counter)
                           comment-char)))
          (setq content (concat content end-marker)))))
    
    ;; Return (content, start-pos, end-pos)
    (list content editable-start editable-end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 8: APPLY READ-ONLY PROTECTION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--apply-readonly-protection (buffer related-blocks current-element comment-char)
  "Apply read-only text properties to non-editable regions.

BUFFER is the editing buffer.
RELATED-BLOCKS is the list of block info.
CURRENT-ELEMENT is the current editable block.
COMMENT-CHAR is the comment character.

This function marks all regions except the current block as read-only,
by iterating through the buffer and finding block markers."
  (with-current-buffer buffer
    (let ((block-counter 0)
          (pos 1))
      
      (dolist (block-info related-blocks)
        (setq block-counter (+ block-counter 1))
        (let ((is-current (eq (plist-get block-info :element) current-element))
              (block-label (if (= (length related-blocks) 1) "Block" block-counter)))
          
          ;; If this is not the current block, protect it
          (unless is-current
            ;; Find the begin marker for this block
            (let* ((begin-marker-text (format "[%s - Read Only" block-label))
                   (begin-marker-pos (save-excursion
                                       (goto-char pos)
                                       (search-forward begin-marker-text nil t)))
                   (end-marker-text (format "[End %s]" block-label))
                   (end-marker-pos (when begin-marker-pos
                                     (search-forward end-marker-text nil t))))
              
              (when (and begin-marker-pos end-marker-pos)
                ;; Protect from the beginning of the begin-marker line
                ;; to the end of the end-marker line
                (let ((line-start (save-excursion
                                    (goto-char begin-marker-pos)
                                    (line-beginning-position)))
                      (line-end (save-excursion
                                  (goto-char end-marker-pos)
                                  (line-end-position))))
                  (put-text-property line-start line-end 'read-only t)
                  (setq pos line-end)))))))))

  ;; Return markers for editable region
  (let* ((editable-begin-text "[Block - EDITABLE")
         (editable-end-text "[End Block]")
         (begin-pos (save-excursion
                      (goto-char (point-min))
                      (search-forward editable-begin-text nil t)))
         (end-pos (when begin-pos
                    (search-forward editable-end-text nil t))))
    (when (and begin-pos end-pos)
      (list (copy-marker begin-pos :insert-after)
            (copy-marker end-pos :insert-before)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 9: EXTRACT EDITABLE CONTENT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--extract-editable-content (buffer editable-begin editable-end)
  "Extract the actual code content from the editable region.

BUFFER is the editing buffer.
EDITABLE-BEGIN and EDITABLE-END are markers pointing to the editable region.

Returns the cleaned code content (without markers)."
  (with-current-buffer buffer
    (let* ((begin-pos (marker-position editable-begin))
           (end-pos (marker-position editable-end))
           (content (buffer-substring-no-properties begin-pos end-pos)))
      
      ;; Clean up: remove begin and end marker lines
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        
        ;; Remove begin marker line
        (when (search-forward "[Block" nil t)
          (let ((line-start (save-excursion (forward-line 0) (point)))
                (line-end (save-excursion (forward-line 1) (point))))
            (delete-region line-start line-end)))
        
        ;; Remove end marker line if present
        (goto-char (point-min))
        (when (search-forward "[End Block]" nil t)
          (let ((line-start (save-excursion (forward-line 0) (point)))
                (line-end (save-excursion (forward-line 1) (point))))
            (delete-region line-start line-end)))
        
        ;; Return cleaned content
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 10: UPDATE ORG BLOCK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--update-org-block (org-buffer org-marker new-content)
  "Update the source block in the Org file with new content.

ORG-BUFFER is the original Org file buffer.
ORG-MARKER is a marker pointing to the block in the Org file.
NEW-CONTENT is the cleaned code content to insert."
  (with-current-buffer org-buffer
    (when-let* ((org-pos (marker-position org-marker)))
      (save-excursion
        ;; Go to the block position
        (goto-char org-pos)
        
        ;; Get the current block element to find its boundaries
        (when-let* ((element (org-element-at-point))
                    (elem-type (org-element-type element)))
          (when (eq elem-type 'src-block)
            ;; Get the :value property boundaries
            ;; The :value contains the actual code, we need to find it
            (let* ((begin (org-element-property :begin element))
                   (end (org-element-property :end element))
                   ;; Find the actual start of the body (after the #+begin_src line)
                   (body-start (save-excursion
                                 (goto-char begin)
                                 (forward-line 1)
                                 (point)))
                   ;; Find the end of the body (before #+end_src)
                   (body-end (save-excursion
                               (goto-char end)
                               (forward-line -1)
                               (line-end-position))))
              
              ;; Delete old body content
              (delete-region body-start body-end)
              
              ;; Insert new content
              (goto-char body-start)
              (insert new-content)
              
              ;; Ensure proper formatting
              (unless (string-suffix-p "\n" new-content)
                (insert "\n"))
              
              ;; Mark buffer as modified
              (set-buffer-modified-p t)))))))
  
  ;; Return the updated marker position
  org-marker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 11: SETUP LANGUAGE MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--setup-language-mode (language)
  "Setup the appropriate major mode for the given LANGUAGE.

LANGUAGE is a string like 'python', 'javascript', etc.

This attempts to enable the correct mode based on language.
Falls back to fundamental-mode if no matching mode is found."
  (let ((mode-name (intern (format "%s-mode" language))))
    (if (fboundp mode-name)
        (funcall mode-name)
      ;; Try common alternatives
      (cond
       ((string-equal language "shell") (bash-mode))
       ((string-equal language "elisp") (emacs-lisp-mode))
       ((string-equal language "javascript") (js-mode))
       ((string-equal language "typescript") (typescript-mode))
       (t (fundamental-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 12: MAIN USER COMMAND - OPEN FOR EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-src-edit-enhanced ()
  "Open the source block at point for enhanced editing.

This command:
  1. Extracts information about the current source block
  2. Collects all related blocks (if using tangle or session)
  3. Creates/opens a dedicated editing buffer
  4. Sets up proper syntax highlighting and read-only protection
  5. Displays the editing buffer

Related blocks are determined by:
  - If the block has :tangle, collects all blocks with the same tangle target
  - If the block has :session, collects all blocks in the same session
  - Otherwise, opens only the current block

Use `org-src-edit-enhanced-save' to save changes back to the Org file,
or `org-src-edit-enhanced-abort' to discard changes."
  (interactive)
  
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  
  ;; Get current block info
  (let* ((block-info (org-src-edit--get-block-info-at-point)))
    (unless block-info
      (user-error "Not on a source block"))
    
    (let* ((language (plist-get block-info :language))
           (org-file (buffer-file-name))
           (session (plist-get block-info :session))
           (tangle (plist-get block-info :tangle))
           (dir-from-block (plist-get block-info :dir))
           (effective-dir (org-src-edit--get-effective-dir dir-from-block org-file))
           (current-element (plist-get block-info :element))
           (comment-char (org-src-edit--get-comment-char language))
           (target-file (org-src-edit--get-target-file language session tangle effective-dir org-file)))
      
      ;; Ensure target file exists
      (org-src-edit--ensure-file-exists target-file)
      
      ;; Collect related blocks
      (let ((related-blocks (org-src-edit--collect-related-blocks block-info (current-buffer))))
        (unless related-blocks
          (setq related-blocks (list block-info)))
        
        ;; Build edit content
        (pcase-let ((`(,content ,edit-start ,edit-end)
                     (org-src-edit--build-edit-content related-blocks current-element comment-char)))
          
          ;; Create or get editing buffer
          (let ((edit-buffer (find-file-noselect target-file)))
            (with-current-buffer edit-buffer
              ;; Clear and insert new content
              (erase-buffer)
              (insert content)
              
              ;; Setup language mode
              (org-src-edit--setup-language-mode language)
              
              ;; Apply read-only protection to non-editable blocks
              (org-src-edit--apply-readonly-protection
               edit-buffer related-blocks current-element comment-char)
              
              ;; Store session info for later use
              (setq-local org-src-edit--session-info
                          (list :org-buffer (current-buffer)
                                :org-marker (copy-marker (plist-get block-info :begin))
                                :language language
                                :comment-char comment-char
                                :target-file target-file
                                :related-blocks related-blocks
                                :current-element current-element))
              
              ;; Enable minor mode
              (org-src-edit-enhanced-mode 1)
              
              ;; Move to start of editable content
              (when edit-start
                (goto-char (+ edit-start 1)))
              
              ;; Display buffer
              (switch-to-buffer edit-buffer)
              
              ;; Show help message
              (message "Editing %s block. Use C-c C-c to save or C-c C-k to abort."
                       language)))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 13: SAVE CHANGES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-src-edit-enhanced-save ()
  "Save changes and return to the Org file.

This command:
  1. Extracts the edited content
  2. Updates the original block in the Org file
  3. Saves both the edit file and Org file
  4. Returns to the Org file"
  (interactive)
  
  (unless org-src-edit--session-info
    (user-error "Not in an org-src-edit-enhanced buffer"))
  
  (let* ((session-info org-src-edit--session-info)
         (org-buffer (plist-get session-info :org-buffer))
         (org-marker (plist-get session-info :org-marker))
         (editable-begin (plist-get session-info :editable-begin))
         (editable-end (plist-get session-info :editable-end)))
    
    ;; Extract editable content
    (when-let* ((new-content (org-src-edit--extract-editable-content
                              (current-buffer) editable-begin editable-end)))
      
      ;; Update Org file
      (org-src-edit--update-org-block org-buffer org-marker new-content)
      
      ;; Save edit file
      (save-buffer)
      
      ;; Save Org file
      (with-current-buffer org-buffer
        (save-buffer))
      
      ;; Close edit buffer
      (kill-buffer (current-buffer))
      
      ;; Return to Org file
      (switch-to-buffer org-buffer)
      (message "Changes saved and buffer closed."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 14: ABORT EDITING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-src-edit-enhanced-abort ()
  "Abort editing and discard changes.

This command closes the edit buffer without saving changes."
  (interactive)
  
  (unless org-src-edit--session-info
    (user-error "Not in an org-src-edit-enhanced buffer"))
  
  (let* ((session-info org-src-edit--session-info)
         (org-buffer (plist-get session-info :org-buffer)))
    
    ;; Close edit buffer without saving
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))
    
    ;; Return to Org file
    (switch-to-buffer org-buffer)
    (message "Edit aborted, changes discarded.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 15: MINOR MODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar org-src-edit-enhanced-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'org-src-edit-enhanced-save)
    (define-key map (kbd "C-c C-k") #'org-src-edit-enhanced-abort)
    map)
  "Keymap for org-src-edit-enhanced-mode.")

(define-minor-mode org-src-edit-enhanced-mode
  "Minor mode for enhanced org source block editing.

Provides keybindings:
  C-c C-c - Save changes and return to Org file
  C-c C-k - Abort editing without saving"
  :init-value nil
  :lighter " OrgSrcEdit"
  :keymap org-src-edit-enhanced-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 16: PROVIDE PACKAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'org-src-edit-enhanced)

;;; org-src-edit-enhanced.el ends here
