;;; init-lsp-proxy.el --- Init Lsp-proxy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-proxy
  :ensure (lsp-proxy
           :host github
           :repo "jadestrong/lsp-proxy"
           :files ("*.el")
           :build (:not compile))
  :if(1043/enable-lsp-proxy-p)
  :bind (:map lsp-proxy-mode-map
              ("M-." . lsp-proxy-find-definition)
              ("M-?" . lsp-proxy-find-references))


  :hook ((org-mode)
         (python-mode)
         (python-ts-mode)
         (tsx-ts-mode)
         (js-ts-mode)
         (typescript-mode)
         (typescript-ts-mode))

  :config
  (setq lsp-proxy-server-path
        (expand-file-name "lsp-proxy/node_modules/.bin/emacs-lsp-proxy" user-emacs-directory))

  ;; Enable LSP support in org-babel code blocks
  (setq lsp-proxy-enable-org-babel t)

  ;; Enable LSP support in org-edit-special buffers (default: t)
  (setq lsp-proxy-org-edit-special-enable-lsp t)

  ;; Specify which languages to enable LSP support for in org-babel blocks
  (setq lsp-proxy-org-babel-enabled-languages
        '("python" "typescript" "javascript" "tsx" "bash" "rust" "go"))

  ;; Map org-babel language names to LSP language IDs
  (setq lsp-proxy-org-babel-language-map
        '(("shell" . "bash")
          ("sh" . "bash")
          ("tsx-ts" . "tsx")
          ("typescript-ts" . "typescript")))

  ;; 日志信息记录等级
  (setq lsp-proxy-log-level 0)
  ;; 选择提供诊断信息的包
  (setq lsp-proxy-diagnostics-provider :auto))

(provide 'init-lsp-proxy)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-lsp-proxy.el ends here
