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

;; (setq org-src-edit-language-extensions nil)  ;; 清空缓存
;; (setq org-src-edit-comment-styles nil)

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
  "Get file extension for the given LANGUAGE."
  (if (not language)
      "txt"
    (let* ((lang-lower (downcase language))
           (lang-keyword (intern (concat ":" lang-lower)))
           ;; 【DEBUG】查看当前的配置
           (_debug-config (progn
                            (message "【get-extension】config = %S"
                                     org-src-edit-language-extensions)
                            nil))
           (ext (plist-get org-src-edit-language-extensions lang-keyword)))

      ;; 【DEBUG】输出查找结果
      (message "【get-extension】lang='%s' → keyword='%s' → ext='%s'"
               language lang-keyword (or ext "NOT FOUND"))

      (or ext "txt"))))

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

LANGUAGE: programming language (string)
SESSION: session name from :session arg or nil
TANGLE: explicit filename from :tangle arg or nil
EFFECTIVE-DIR: effective directory for the file
ORG-FILE: path to the Org file

Returns the full file path where editing will happen."

  (if tangle
      ;; Case 1: 用户明确指定了 :tangle 参数
      (expand-file-name tangle (or effective-dir (file-name-directory org-file)))

    ;; Case 2: 自动生成文件名
    (let* ((identifier (org-src-edit--file-identifier org-file))
           (ext (org-src-edit--get-extension language))

           ;; ✅ 添加调试
           (_debug-get-ext (progn
                             (message "【get-target-file】语言='%s' → 扩展名='%s'" language ext)
                             ext))

           ;; 文件夹名称：包含识别符、语言和可选的会话
           (folder-name (if session
                            (format "%s-%s-%s" identifier language session)
                          (format "%s-%s" identifier language)))

           ;; 根目录
           (root (org-src-edit--expand-tilde org-src-edit-root-dir))
           (project-dir (concat (file-name-as-directory root) folder-name))

           ;; 完整的文件路径
           (target-path (concat (file-name-as-directory project-dir)
                                org-src-edit-entry-filename "." ext)))

      ;; ✅ 最后的调试输出
      (progn
        (message "【get-target-file】最终路径: %s" target-path)
        target-path))))

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
  :language - Programming language (string, lowercase)
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
  ;; 获取当前元素
  (let ((element (org-element-at-point)))
    ;; 检查是否真的是 src-block
    (when (eq (org-element-type element) 'src-block)
      ;; 获取语言 - 这是关键部分！
      (let* ((lang-raw (org-element-property :language element))
             ;; 确保语言名称被正确转小写
             (language (when lang-raw (downcase lang-raw))))

        ;; 只有当我们成功获取语言时才继续
        (when language
          ;; 现在获取其他信息
          (let ((body (org-element-property :value element)))

            ;; 调用 org-babel-get-src-block-info 获取 header args
            (when-let* ((info (org-babel-get-src-block-info 'no-eval element)))
              ;; info 结构：(language body arguments switches name start coderef)
              (pcase-let ((`(,_info-lang ,_info-body ,arguments ,_switches ,name ,_start ,_coderef) info))
                ;; 返回包含所有信息的 plist
                (list
                 :language language
                 :body body
                 :session (plist-get arguments :session)
                 :tangle (plist-get arguments :tangle)
                 :dir (plist-get arguments :dir)
                 :begin (org-element-property :begin element)
                 :end (org-element-property :end element)
                 :line-number (line-number-at-pos (org-element-property :begin element))
                 :name name
                 :element element)))))))))

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
;; ✅ SECTION 7 (修订): BUILD EDIT CONTENT - 精确的可编辑区域边界
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--build-edit-content (related-blocks current-element comment-char)
  "Build the content for the editing buffer.

RELATED-BLOCKS is a list of block-info plists from org-src-edit--collect-related-blocks.
CURRENT-ELEMENT is the org-element of the current (editable) block.
COMMENT-CHAR is the comment character for this language.

Returns a list (CONTENT EDITABLE-BEGIN EDITABLE-END) where:
  CONTENT is the complete buffer content string
  EDITABLE-BEGIN is the position (integer) where editable region starts (AFTER markers)
  EDITABLE-END is the position (integer) where editable region ends (BEFORE markers)"
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

        ;; Add begin marker
        (setq content (concat content begin-marker))

        ;; 【关键】记录可编辑区域的开始：marker 之后
        (when is-current
          (setq editable-start (length content)))

        ;; Add body (ensure it ends with newline)
        (let ((body-with-newline (if (string-suffix-p "\n" body)
                                     body
                                   (concat body "\n"))))
          (setq content (concat content body-with-newline))

          ;; 【关键】记录可编辑区域的结束：body 之后、end marker 之前
          (when is-current
            (setq editable-end (length content))))

        ;; Add end marker
        (let ((end-marker (org-src-edit--make-end-marker
                           (if (= (length related-blocks) 1) "Block" block-counter)
                           comment-char)))
          (setq content (concat content end-marker)))))

    ;; Return (content, start-pos, end-pos)
    ;; These positions point to the BODY CONTENT only, excluding markers
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
;; ✅ SECTION 9 (修订): EXTRACT EDITABLE CONTENT - 精确提取
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--extract-editable-content (buffer editable-begin editable-end)
  "Extract the actual code content from the editable region.

BUFFER is the editing buffer.
EDITABLE-BEGIN and EDITABLE-END are markers pointing to the editable region.

Returns the cleaned code content (without markers).

The markers should point to:
  EDITABLE-BEGIN: position right AFTER the begin marker line
  EDITABLE-END: position right BEFORE the end marker line"
  (with-current-buffer buffer
    (save-excursion
      ;; 获取 marker 的位置
      (let* ((begin-pos (marker-position editable-begin))
             (end-pos (marker-position editable-end)))

        (unless (and begin-pos end-pos (< begin-pos end-pos))
          (user-error "【extract】错误: Editable region markers are invalid (begin=%s, end=%s)"
                      begin-pos end-pos))

        ;; 【关键】直接从缓冲区提取文本（这应该是纯代码，没有 markers）
        (let ((raw-content (buffer-substring begin-pos end-pos)))

          ;; ✅ 验证：不应该包含 marker 标记
          (when (string-match-p "\\[.*Block.*\\]" raw-content)
            (message "【⚠️ WARNING】提取的内容包含 marker 标记！这表示边界设置有问题")
            (message "内容预览: %S" (substring raw-content 0 (min 100 (length raw-content)))))

          ;; ✅ 清理内容：移除开头和末尾的空白/换行
          (with-temp-buffer
            (insert raw-content)
            
            ;; 移除开头的空白行
            (goto-char (point-min))
            (while (and (< (point) (point-max))
                        (string-match-p "^[[:space:]]*$"
                                        (buffer-substring (line-beginning-position)
                                                          (line-end-position))))
              (delete-region (line-beginning-position) 
                             (min (+ (line-end-position) 1) (point-max))))

            ;; 移除末尾的空白行
            (goto-char (point-max))
            (while (and (> (point) (point-min))
                        (string-match-p "^[[:space:]]*$"
                                        (buffer-substring (line-beginning-position)
                                                          (line-end-position))))
              (delete-region (max (- (line-beginning-position) 1) (point-min)) 
                             (line-end-position)))

            ;; ✅ 返回清理后的内容（不包含 markers）
            (buffer-substring-no-properties (point-min) (point-max))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 10 (升级版): UPDATE ORG BLOCK - 带有恢复机制
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-src-edit--find-block-by-line (org-buffer line-number language body-hash)
  "Find a src-block in the Org buffer by line number and other identifiers.

ORG-BUFFER: the Org file buffer
LINE-NUMBER: the line number where the block should be
LANGUAGE: the language of the block
BODY-HASH: MD5 hash of the block body (for verification)

Returns the element if found, nil otherwise."
  (with-current-buffer org-buffer
    (save-excursion
      ;; 解析缓冲区
      (let ((tree (org-element-parse-buffer 'element)))
        ;; 查找所有 src-block
        (let (result-block)
          (org-element-map tree 'src-block
            (lambda (block)
              (unless result-block
                (let* ((block-line (line-number-at-pos (org-element-property :begin block)))
                       (block-lang (downcase (org-element-property :language block)))
                       (block-body (org-element-property :value block))
                       (block-hash (md5 (or block-body ""))))

                  ;; 匹配条件：行号 + 语言 + 哈希
                  (when (and (= block-line line-number)
                             (string-equal block-lang language)
                             (string-equal block-hash body-hash))
                    (setq result-block block))))))

          result-block)))))

(defun org-src-edit--update-org-block (org-buffer org-marker new-content 
                                                  &optional backup-line backup-language backup-hash)
  "Update the source block in the Org file with new content.

ORG-BUFFER: the original Org file buffer
ORG-MARKER: a marker pointing to the block in the Org file (primary method)
NEW-CONTENT: the cleaned code content to insert
BACKUP-LINE: backup line number (for recovery if marker fails)
BACKUP-LANGUAGE: backup language (for recovery)
BACKUP-HASH: backup body hash (for recovery)

This function uses multiple strategies to find and update the block:
  1. Primary: Use the marker directly
  2. Fallback 1: Use line number + language + hash
  3. Fallback 2: Raise error with diagnostic info"
  
  (with-current-buffer org-buffer
    ;; ✅ 策略 1: 尝试使用 marker
    (let ((element (when-let* ((org-pos (marker-position org-marker)))
                     (save-excursion
                       (goto-char org-pos)
                       (org-element-at-point)))))

      ;; 验证 marker 是否仍然指向 src-block
      (if (and element (eq (org-element-type element) 'src-block))
          ;; ✅ Marker 仍然有效
          (org-src-edit--do-update-block org-buffer element new-content)

        ;; ❌ Marker 失效，尝试恢复
        (if (and backup-line backup-language backup-hash)
            (progn
              (message "【update-org-block】⚠️  Marker 失效，尝试使用备用标识符恢复...")
              (let ((recovered-element 
                     (org-src-edit--find-block-by-line org-buffer backup-line 
                                                       backup-language backup-hash)))
                (if recovered-element
                    (progn
                      (message "【update-org-block】✅ 成功恢复块，继续更新...")
                      (org-src-edit--do-update-block org-buffer recovered-element new-content))

                  ;; 恢复也失败了
                  (user-error "【update-org-block】错误: 无法通过 marker 或备用标识符找到块"))))

          ;; 没有备用信息
          (user-error "【update-org-block】错误: Marker 无效且没有备用标识符"))))))

(defun org-src-edit--do-update-block (org-buffer element new-content)
  "Internal function to perform the actual block update.

ORG-BUFFER: the Org file buffer
ELEMENT: the org-element object of the src-block
NEW-CONTENT: the new content to insert

This is factored out from org-src-edit--update-org-block so we can reuse
the update logic regardless of how the element was found."
  (with-current-buffer org-buffer
    (save-excursion
      ;; 获取块的边界
      (let* ((block-begin (org-element-property :begin element))
             (block-end (org-element-property :end element))
             (block-line (line-number-at-pos block-begin)))

        ;; 找到 body 的起始位置（header 后的第一行）
        (let ((body-start (save-excursion
                            (goto-char block-begin)
                            (line-end-position)
                            (forward-char 1)
                            (point)))

              ;; 找到 #+end_src 行的前一行末尾
              (body-end (save-excursion
                          (goto-char block-end)
                          (forward-line -1)
                          (line-end-position))))

          ;; 验证边界
          (unless (and (> body-start block-begin)
                       (> body-end body-start)
                       (<= body-end block-end))
            (user-error "【update-org-block】错误: 块边界计算失败"))

          ;; 删除旧的代码体
          (delete-region body-start body-end)

          ;; 插入新内容
          (goto-char body-start)
          (insert new-content)

          ;; 确保新内容以换行结尾
          (unless (string-suffix-p "\n" new-content)
            (insert "\n"))

          ;; 标记缓冲区为已修改
          (set-buffer-modified-p t)

          ;; 调试输出
          (message "【update-org-block】✅ 成功更新第 %d 行的代码块（%d 字符）"
                   block-line
                   (length new-content)))))))


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
;; ✅ SECTION 12 (修订): 创建 Markers 并保存冗余信息用于恢复
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-src-edit-enhanced ()
  "Open the source block at point for enhanced editing.

This function:
  1. Extracts block information at point
  2. Collects related blocks if needed
  3. Creates an edit buffer with proper markers
  4. Stores session info including backup identifiers for recovery"
  (interactive)

  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))

  ;; Get current block info
  (let* ((block-info (org-src-edit--get-block-info-at-point)))
    (unless block-info
      (user-error "Not on a source block"))

    (let* ((language (plist-get block-info :language))
           (_debug-1 (progn
                       (message "【DEBUG 1】语言检测结果: '%s' (类型: %s)"
                                language
                                (type-of language))
                       language))

           (org-file (buffer-file-name))
           (session (plist-get block-info :session))
           (tangle (plist-get block-info :tangle))
           (dir-from-block (plist-get block-info :dir))
           (effective-dir (org-src-edit--get-effective-dir dir-from-block org-file))
           (current-element (plist-get block-info :element))
           (comment-char (org-src-edit--get-comment-char language))

           (ext (org-src-edit--get-extension language))
           (_debug-2 (progn
                       (message "【DEBUG 2】获取的扩展名: '%s'" ext)
                       ext))

           (target-file (org-src-edit--get-target-file language session tangle effective-dir org-file))

           (_debug-3 (progn
                       (message "【DEBUG 3】目标文件路径: '%s'" target-file)
                       target-file))

           (org-buffer (current-buffer))

           ;; ✅ 新增：获取块的行号（用于恢复）
           (block-line-number (plist-get block-info :line-number))
           (block-body (plist-get block-info :body)))

      ;; 确保目标文件存在
      (org-src-edit--ensure-file-exists target-file)

      (let ((_debug-4 (progn
                        (if (file-exists-p target-file)
                            (message "【DEBUG 4】✅ 文件已创建: %s" target-file)
                          (message "【DEBUG 4】❌ 文件创建失败: %s" target-file))
                        target-file)))

        ;; 收集相关块
        (let ((related-blocks (org-src-edit--collect-related-blocks block-info org-buffer)))
          (unless related-blocks
            (setq related-blocks (list block-info)))

          ;; 构建编辑内容
          (pcase-let ((`(,content ,edit-start ,edit-end)
                       (org-src-edit--build-edit-content related-blocks current-element comment-char)))

            ;; 创建或获取编辑缓冲区
            (let ((edit-buffer (find-file-noselect target-file)))
              (message "【DEBUG 5】打开的缓冲区: %s" (buffer-name edit-buffer))

              (with-current-buffer edit-buffer
                ;; 清除并插入新内容
                (erase-buffer)
                (insert content)

                ;; 设置语言模式
                (org-src-edit--setup-language-mode language)

                ;; 应用只读保护
                (org-src-edit--apply-readonly-protection
                 edit-buffer related-blocks current-element comment-char)

                ;; 创建可编辑区域的 markers
                (let ((editable-begin-marker (copy-marker (+ edit-start 1) t))
                      (editable-end-marker (copy-marker edit-end t)))

                  ;; ✅ 保存会话信息（包含恢复用的备用信息）
                  (setq-local org-src-edit--session-info
                              (list :org-buffer org-buffer
                                    ;; 主要方式：使用 marker
                                    :org-marker (copy-marker (plist-get block-info :begin))
                                    ;; 备用方式 1：使用行号
                                    :block-line-number block-line-number
                                    ;; 备用方式 2：使用内容摘要
                                    :block-body-hash (md5 (or block-body ""))
                                    :block-language language
                                    ;; 其他信息
                                    :language language
                                    :comment-char comment-char
                                    :target-file target-file
                                    :related-blocks related-blocks
                                    :current-element current-element
                                    :editable-begin editable-begin-marker
                                    :editable-end editable-end-marker))

                  ;; 启用 minor mode
                  (org-src-edit-enhanced-mode 1)

                  ;; 移动到可编辑内容的开始
                  (goto-char (marker-position editable-begin-marker))

                  ;; 显示缓冲区
                  (switch-to-buffer edit-buffer)

                  ;; 显示帮助信息
                  (message "Editing %s block. Use C-c C-c to save or C-c C-k to abort."
                           language))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ✅ SECTION 13 (修订): SAVE CHANGES - 使用备用参数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-src-edit-enhanced-save ()
  "Save changes and return to the Org file.

This function implements a robust save sequence:
  1. Extract content from the edit buffer
  2. Save the edit file to disk
  3. Update the Org buffer's content (with recovery mechanism)
  4. Save the Org file
  5. Close the edit buffer
  6. Return to the Org file"
  (interactive)

  (unless org-src-edit--session-info
    (user-error "Not in an org-src-edit-enhanced buffer"))

  ;; 获取会话信息
  (let* ((session-info org-src-edit--session-info)
         (edit-buffer (current-buffer))
         (org-buffer (plist-get session-info :org-buffer))
         (org-marker (plist-get session-info :org-marker))
         (editable-begin (plist-get session-info :editable-begin))
         (editable-end (plist-get session-info :editable-end))
         ;; ✅ 新增：获取备用标识符
         (backup-line (plist-get session-info :block-line-number))
         (backup-language (plist-get session-info :block-language))
         (backup-hash (plist-get session-info :block-body-hash)))

    ;; ✅ STEP 1: 提取编辑内容
    (message "【save】STEP 1: 提取编辑内容...")
    (let ((new-content (org-src-edit--extract-editable-content
                        edit-buffer editable-begin editable-end)))

      (unless new-content
        (user-error "Failed to extract content from edit buffer"))

      (message "【save】STEP 1: ✅ 提取到 %d 字符" (length new-content))

      ;; ✅ STEP 2: 保存编辑文件到磁盘
      (message "【save】STEP 2: 保存编辑文件...")
      (with-current-buffer edit-buffer
        (save-buffer))
      (message "【save】STEP 2: ✅ 编辑文件已保存")

      ;; ✅ STEP 3: 验证 Org 缓冲区存活
      (unless (buffer-live-p org-buffer)
        (user-error "Original Org buffer has been killed"))

      ;; ✅ STEP 4: 在 Org 缓冲区中更新内容（带备用参数）
      (message "【save】STEP 3: 更新 Org 缓冲区...")
      (org-src-edit--update-org-block org-buffer org-marker new-content
                                      backup-line backup-language backup-hash)
      (message "【save】STEP 3: ✅ Org 缓冲区已更新")

      ;; ✅ STEP 5: 保存 Org 文件
      (message "【save】STEP 4: 保存 Org 文件...")
      (with-current-buffer org-buffer
        (save-buffer))
      (message "【save】STEP 4: ✅ Org 文件已保存")

      ;; ✅ STEP 6: 标记编辑缓冲区为未修改
      (with-current-buffer edit-buffer
        (set-buffer-modified-p nil))

      ;; ✅ STEP 7: 关闭编辑缓冲区
      (message "【save】STEP 5: 关闭编辑缓冲区...")
      (kill-buffer edit-buffer)
      (message "【save】STEP 5: ✅ 编辑缓冲区已关闭")

      ;; ✅ STEP 8: 切换回 Org 缓冲区
      (message "【save】STEP 6: 返回 Org 缓冲区...")
      (if (buffer-live-p org-buffer)
          (progn
            (switch-to-buffer org-buffer)
            (message "【save】STEP 6: ✅ 已返回 Org 文件"))
        (user-error "Cannot return to Org buffer")))))

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
