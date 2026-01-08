;;; init-1043.el --- Init-1043 -*- lexical-binding: t; -*-
;;; Commentary: 程序调用函数及变量
;;; Code:

(setq user-full-name "ghostlou1043"
      user-mail-address "ghostlou1043@gmail.com"
      auth-sources '("~/.authinfo.gpg"))

;; Select System Scale.
;; https://mydevice.alsacreations.com/
;; window.devicePixelRatio
(defvar 1043/system-scale '100
  "The master switch for system-scale
   Options: '100, '125, '150, '175, '200")

(defun 1043/system-scale-100-p ()
  (eq 1043/system-scale '100))

(defun 1043/system-scale-125-p ()
  (eq 1043/system-scale '125))

(defun 1043/system-scale-150-p ()
  (eq 1043/system-scale '150))

(defun 1043/system-scale-175-p ()
  (eq 1043/system-scale '175))

(defun 1043/system-scale-200-p ()
  (eq 1043/system-scale '200))

;; Select Theme.
(defvar 1043/themes 'built-in
  "The master switch for themes.
   Options: 'modus-themes, 'built-in")

(defun 1043/enable-built-in-themes-p ()
  "Returns t if built-in is the selected theme."
  (eq 1043/themes 'built-in))

(defun 1043/enable-modus-themes-p ()
  "Returns t if modus-themes is the selected theme."
  (eq 1043/themes 'modus-themes))


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

;; Select File Manager.
(defvar 1043/file-manager 'dired
  "The master switch for file manager.
   Options: 'dirvish, 'dired")

(defun 1043/enable-drivish-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/file-manager 'dirvish))

(defun 1043/enable-dired-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/file-manager 'dired))

;; Select Input Method.
(defvar 1043/input-method 'rime
  "The master switch for input method.
   Options: 'rime, 'pyim")

(defun 1043/enable-rime-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/input-method 'rime))

(defun 1043/enable-pyim-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/input-method 'pyim))

;; Project Management.
(defvar 1043/project-management 'project
  "The master switch for project management.
   Options: 'project, 'projectile")

(defun 1043/enable-project-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/project-management 'project))

(defun 1043/enable-projectile-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/project-management 'projectile))


;; File.
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

;; Fonts.
(require 'seq)

(defvar 1043/font-family-list nil
  "Cache font family list to avoid repeated calls.")

(defun 1043/set-font-family-list ()
  "Initialize cached font family list."
  (setq 1043/font-family-list (font-family-list)))  

(defun 1043/set-font-if-exists (script font-family)
  "If FONT-FAMILY exists, set it for SCRIPT."
  (when (member font-family 1043/font-family-list)
    (set-fontset-font t script (font-spec :family font-family))))

(defun 1043/get-emoji-rescale ()
  "Return a suitable emoji rescale factor based on current default font height."
  (let ((height (face-attribute 'default :height)))
    (cond
     ((<= height 105) 0.82) ;; s ok 14 11 3
     ((<= height 120) 0.82) ;; s ok 16 13 3
     ((<= height 150) 0.82) ;; r ok 20 16 4
     ((<= height 180) 0.82) ;; h ok 24 19 5
     ((<= height 195) 0.82) ;; t ok 26 21 5
     ((<= height 225) 0.82) ;; p ok 30 24 6
     ((<= height 241) 0.82) ;; p ok 32 26 6
     (t               0.82))))

(defun 1043/update-emoji-rescale ()
  "Update emoji font rescale according to current default font height."
    (when (member "Noto Color Emoji" 1043/font-family-list)
      (setq face-font-rescale-alist
            (seq-remove (lambda (entry)
                          (let ((font-pattern (car entry)))
                            (and (fontp font-pattern)
                                 (string-equal (font-get font-pattern :family) "Noto Color Emoji"))))
                        face-font-rescale-alist)))
    (add-to-list 'face-font-rescale-alist
                 (cons (font-spec :family "Noto Color Emoji") (1043/get-emoji-rescale))
                 t))

(defun 1043/redraw-emoji-mono () ;; fontaine-set-preset-hook
  (1043/get-emoji-rescale)
  (1043/update-emoji-rescale)
  (clear-face-cache t) ;; 其实nil也行
  (redraw-frame))

(provide 'init-1043)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-1043.el ends here
