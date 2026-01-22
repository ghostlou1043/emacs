;;; init-lsp-proxy.el --- Init Lsp-proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-proxy
  :ensure (lsp-proxy
           :host github
           :repo "jadestrong/lsp-proxy"
           :files ("*.el"))
  :if(1043/enable-lsp-proxy-p)
  :init
  (setq lsp-proxy--exec-file
        (expand-file-name "lsp-proxy/node_modules/.bin/emacs-lsp-proxy" user-emacs-directory))
  :hook ((python-ts-mode)
         (tsx-ts-mode)
         (js-ts-mode)
         (typescript-mode)
         (typescript-ts-mode))

  :config
  (setq lsp-proxy-log-level 0)
  (setq lsp-proxy-diagnostics-provider :auto))



(provide 'init-lsp-proxy)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-lsp-proxy.el ends here
