;;; core-eglot.el --- Core Eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package eglot
  :ensure nil                    ;; eglot 是内置的，不需要额外下载
  :if (1043/enable-eglot-p)
  ;; :bind
  ;; eglot
  ;; eglot-reconnect
  ;; eglot-shutdown
  ;; eglot-shutdown-all
  ;; eglot-rename
  ;; eglot-format
  ;; eglot-format-buffer
  ;; eglot-code-actions
  ;; eglot-code-action-organize-imports
  ;; eglot-code-action-quickfix
  ;; eglot-code-action-extract
  ;; eglot-code-action-inline
  ;; eglot-code-action-rewrite
  ;; eglot-inlay-hints-mode
  ;; eglot-momentary-inlay-hints
  ;; eglot-semantic-tokens-mode
  ;; eglot-show-type-hierarchy
  ;; eglot-call-type-hierarchy
  ;; eglot-events-buffer
  ;; eglot-stderr-buffer
  ;; eglot-forget-pending-continuations
  ;; eglot-signal-didChangeConfiguration
  ;; eglot-clear-status
  ;; eldoc
  ;; flymake-show-buffer-diagnostics
  ;; flymake-show-project-diagnostics
  ;; xref-find-definitions
  ;; imenu
  ;; completion-at-point


  :hook
  (python-mode . eglot-ensure)   ;; 当打开 python 文件时，自动启动 eglot
  (python-ts-mode . eglot-ensure)   ;; 当打开 python 文件时，自动启动 eglot
  :config
  ;; Customization Variables
  ;; eglot-autoreconnect
  ;; eglot-connect-timeout
  ;; eglot-sync-connect
  ;; eglot-events-buffer-config 将其 :size 属性设置为 0。这将禁用事件记录，并可能提高速度。
  ;; eglot-autoshutdown
  ;; eglot-confirm-server-edits
  ;; eglot-ignored-server-capabilities
  ;; eglot-extend-to-xref
  ;; eglot-report-progress
  ;; eglot-advertise-cancellation
  ;; eglot-code-action-indications
  ;; eglot-code-action-indicator
  ;; eglot-mode-line-format

  ;; Other Variables
  ;; eglot-server-programs
  ;; eglot-strict-mode
  ;; eglot-server-initialized-hook
  ;; eglot-connect-hook
  ;; eglot-managed-mode-hook
  ;; eglot-stay-out-of

  ;; https://elpa.gnu.org/devel/doc/eglot.html#More-Customization-1


  ;; (setq eglot-ignored-server-capabilities nil)

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
