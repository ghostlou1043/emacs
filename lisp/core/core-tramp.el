;;; core-tramp.el --- Core Tramp -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tramp-hlo
  :ensure t
  :config
  (tramp-hlo-setup))

;; (use-package tramp-rpc
;;   :ensure t
;;   :config
;;   ;; (setq tramp-rpc-deploy-backend 'python) 
;;   )


(provide 'core-tramp)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-tramp.el ends here
