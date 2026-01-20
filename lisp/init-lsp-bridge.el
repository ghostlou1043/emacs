;;; init-lsp-bridge.el --- Init Lsp-bridge -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package lsp-bridge
  :ensure (lsp-bridge
           :host github
           :repo "manateelazycat/lsp-bridge"
           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not compile))
  :if (1043/enable-lsp-bridge-p)
  :init
  (setq lsp-bridge-python-command
        (expand-file-name "~/.config/emacs/lsp-bridge/"))
  (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-lsp-bridge.el ends here
