;;; core-revert.el --- Core-Revert -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t)
  :config
  (setq revert-without-query (list "."))  ; Do not prompt
  (setq auto-revert-stop-on-user-input nil)
  ;; Revert other buffers (e.g, Dired)
  (setq global-auto-revert-non-file-buffers t)
  (setq global-auto-revert-ignore-modes '(Buffer-menu-mode)))  ; Resolve issue #29

(provide 'core-revert)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-revert.el ends here
