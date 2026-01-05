;;; core-gcmh.el --- Core-GCMH -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

(use-package gcmh
  :ensure t
  :delight
  :init
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'gcmh-mode)
    (add-hook 'after-init-hook #'gcmh-mode))
  (setq gcmh-idle-delay 'auto)
  (setq gcmh-auto-idle-delay-factor 10)
  (setq gcmh-high-cons-threshold (* 256 1024 1024)))

(provide 'core-gcmh)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-gcmh.el ends here
