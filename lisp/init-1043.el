;;; init-1043.el --- Init-1043 -*- lexical-binding: t; -*-
;;; Commentary: 程序调用函数及变量
;;; Code:

(setq user-full-name "ghostlou1043"
      user-mail-address "ghostlou1043@gmail.com"
      auth-sources '("~/.authinfo.gpg"))

;; Select Completion Backend.
(defvar 1043/completion-backend 'none
  "The master switch for completion backend.
   Options: 'lsp-bridge, 'lsp-proxy, 'lsp-mode, 'eglot, 'none")

(defun 1043/enable-lsp-bridge-p ()
  "Returns t if lsp-bridge is the selected completion backend."
  (eq 1043/completion-backend 'lsp-bridge))

(defun 1043/enable-lsp-proxy-p ()
  "Returns t if lsp-proxy is the selected completion backend."
  (eq 1043/completion-backend 'lsp-proxy))

(defun 1043/enable-lsp-mode-p ()
  "Returns t if lsp-mode is the selected completion backend."
  (eq 1043/completion-backend 'lsp-mode))

(defun 1043/enable-eglot-p ()
  "Returns t if eglot is the selected completion backend."
  (eq 1043/completion-backend 'eglot))

(defun 1043/enable-corfu-p ()
  "Returns t if corfu should be enabled.
   Enabled for 'none, 'eglot, 'lsp-mode, or 'lsp-proxy."
  (or (eq 1043/completion-backend 'none)
      (eq 1043/completion-backend 'eglot)
      (eq 1043/completion-backend 'lsp-mode)
      (eq 1043/completion-backend 'lsp-proxy)))

;; Select File Manager
(defvar 1043/file-manager 'dired
  "The master switch for file manager.
   Options: 'dirvish, 'dired")

(defun 1043/enable-drivish-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/file-manager 'dirvish))

(defun 1043/enable-dired-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/file-manager 'dired))

;; Select Input Method
(defvar 1043/input-method 'rime
  "The master switch for input method.
   Options: 'rime, 'pyim")

(defun 1043/enable-rime-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/input-method 'rime))

(defun 1043/enable-pyim-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/input-method 'pyim))

;; Project Management
(defvar 1043/project-management 'project
  "The master switch for project management.
   Options: 'project, 'projectile")

(defun 1043/enable-project-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/project-management 'project))

(defun 1043/enable-projectile-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/project-management 'projectile))


;; File
(defun 1043/directory-empty-p (directory)
  "Check if DIRECTORY is empty or contains only .DS_Store."
  (cond
   ((not (file-directory-p directory))
    (message "Directory does not exist: %s" directory)
    nil)
   (t 
    (let ((files (directory-files directory 
                                  nil 
                                  directory-files-no-dot-files-regexp 
                                  t
                                  2))) 
      (null (delete ".DS_Store" files))))))


(provide 'init-1043)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-1043.el ends here
