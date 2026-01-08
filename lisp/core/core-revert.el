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
  :config
  (setq revert-without-query (list ".")) ;; 刷新时无须确认
  (setq auto-revert-verbose t) ;; 设为 nil 减少消息打扰
  (setq auto-revert-interval 3)
  (setq auto-revert-use-notify t)
  (setq auto-revert-check-vc-info t)
  (setq auto-revert-remote-files nil)
  (setq auto-revert-avoid-polling nil)
  (setq auto-revert-stop-on-user-input nil) ;; 即使在输入，也继续刷新
  (setq global-auto-revert-non-file-buffers t)
  ;; Resolve issue #29
  (setq global-auto-revert-ignore-modes '(Buffer-menu-mode)))



(provide 'core-revert)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-revert.el ends here
