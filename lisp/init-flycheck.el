;;; init-flycheck.el --- Init Flycheck -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t
  :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

  (setq flycheck-idle-change-delay 0.5)
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))

  (setq flycheck-indication-mode 'left-fringe)
  (setq flycheck-checker-error-threshold 400)  ; 防止太多提示淹没

  ;; (setq-default flycheck-disabled-checkers '(c/c++-clang))

  )

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(provide 'init-flycheck)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-flycheck.el ends here
