;;; init-apheleia.el --- Init Apheleia -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package apheleia
  :ensure t
  :config
  ;; which formatter to use
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)

  (setq apheleia-hide-log-buffers nil)
  (setq apheleia-log-only-errors t)
  (setq apheleia-max-alignment-size 400)
  (setq apheleia-formatters-respect-indent-level t)

  ;; ;; don't mess up with lsp-mode
  ;; (setq +format-with-lsp nil)
  ;; ;; run the formatter inside container
  ;; (setq apheleia-remote-algorithm 'remote)

  (apheleia-global-mode +1))

(provide 'init-apheleia)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-apheleia.el ends here
