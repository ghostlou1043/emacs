;;; init-1043.el --- Init-1043 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(defvar 1043/file-manager 'dired
  "The master switch for file-manager.
   Options: 'dirvish, 'dired")

(defun 1043/enable-drivish-p ()
  "Returns t if drivish is the selected file manager."
  (eq 1043/completion-backend 'dirvish))

(defun 1043/enable-dired-p ()
  "Returns t if dired is the selected file manager."
  (eq 1043/completion-backend 'dired))


(provide 'init-1043)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-1043.el ends here
