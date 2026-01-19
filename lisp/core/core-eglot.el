;;; core-eglot.el --- Core Eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package eglot
  :ensure nil                    ;; eglot 是内置的，不需要额外下载
  :hook
  (python-mode . eglot-ensure)   ;; 当打开 python 文件时，自动启动 eglot
  (python-ts-mode . eglot-ensure)   ;; 当打开 python 文件时，自动启动 eglot
  :config
  ;; (setq-default eglot-workspace-configuration
  ;;               '((haskell (maxCompletions . 200))))

  ;; 告诉 eglot 遇到 python-mode 时使用 pyright
  (add-to-list 'eglot-server-programs
               `(python-mode . ("pyright-langserver" "--stdio"))))

(provide 'core-eglot)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-eglot.el ends here
