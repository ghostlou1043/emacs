;;; core-language.el --- Core Language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :config
  (setq treesit-extra-load-path nil)
  (setq treesit-language-source-alist
        '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
          (org . ("https://github.com/milisims/tree-sitter-org"))
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
          (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
          (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (php . ("https://github.com/tree-sitter/tree-sitter-php"))
          (nix . ("https://github.com/nix-community/tree-sitter-nix"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (make . ("https://github.com/alemuller/tree-sitter-make"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (java . ("https://github.com/tree-sitter/tree-sitter-java.git"))
          (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
          (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
          (janet . ("https://github.com/GrayJack/tree-sitter-janet"))
          (latex . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
          (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (csharp . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
          (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
          (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))))

(use-package treesit-langs
  :ensure (:host github :repo "emacs-tree-sitter/treesit-langs")
  :demand t
  :config
  (add-hook 'find-file-hook #'treesit-langs-major-mode-setup)) ;; 暂时使用该 hook , 后续根据语言细化

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (setq treesit-auto-install 'prompt)
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode +1))

(use-package kdl-mode
  :ensure t
  :mode "\\.kdl\\'")

(use-package lua-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

;; (use-package cmake-mode
;;   :ensure t
;;   :mode (("CMakeLists\\.txt$" . cmake-mode)
;;          ("\\.cmake$'" . cmake-mode)))

(use-package elisp-mode
  :ensure nil
  :bind
  (:map emacs-lisp-mode-map
	    ("C-c C-f" . nil) ;; elisp-byte-compile-file 
	    ("C-c C-e" . nil))) ;; elisp-eval-region-or-buffer

(use-package markdown-mode
  :ensure t)

;; (use-package eglot
;;   :ensure (:type built-in)
;;   :hook
;;   ((python-mode python-ts-mode). eglot-ensure)
;;   ((go-mode go-ts-mode). eglot-ensure)
;;   ((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode) . eglot-ensure)
;;   ((cmake-mode cmake-ts-mode) . eglot-ensure)
;;   ((yaml-ts-mode yaml-mode) . eglot-ensure)
;;   ((rust-ts-mode rust-mmode) . eglot-ensure)
;;   ((lua-ts-mode lua-mode) . eglot-ensure)
;;   ((markdown-mode) . eglot-ensure)
;;   :bind (:map eglot-mode-map
;;               ("C-c l a" . eglot-code-actions)
;;               ("C-c l r" . eglot-rename)
;;               ("C-c l f" . eglot-format)
;;               ("C-c l d" . eldoc))

;;   :config
;;   (setq eglot-autoshutdown nil)
;;   ;; 在某些情况下，启动语言服务器时，需要提供特定于给定项目的额外信息。变量 eglot-workspace-configuration（请参阅自定义 Eglot）为此目的而存在。它指定了需要传递给每个语言服务器的参数及其值。
;;   (setq eglot-autoreconnect 3)
;;   (setq eglot-connect-timeout 30)
;;   (setq eglot-sync-connect 3)
;;   ;; (setq eglot-confirm-server-edits 如果此选项的值非 nil，Eglot 将在语言服务器建议的编辑操作之前请求确认。可以定制此选项的值，以要求特定命令进行确认，或仅当编辑影响用户尚未访问的文件时进行确认。
;;   ;; (setq eglot-ignored-server-capabilities This variable’s value is a list of language server capabilities that Eglot should not use.
;;   ;; eglot-extend-to-xref
;;   ;; eglot-mode-map
;;   ;; 性能优化
;;   (setq eglot-report-progress nil)
;;   (setq eglot-events-buffer-config 0)
;;   (defvar jsonrpc-log-event-p nil)
;;   (defun jsonrpc--log-event-advice (f &rest args)
;;     (if jsonrpc-log-event-p (apply f args)))
;;   (advice-add #'jsonrpc--log-event :around #'jsonrpc--log-event-advice)

;;   (add-to-list 'eglot-server-programs
;;                '((python-mode python-ts-mode). ("pyright-langserver" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;;                '((lua-mode lua-ts-mode). ("lua-language-server")))
;;   (add-to-list 'eglot-server-programs
;;                '((markdown-mode). ("marksman" "server"))))

;; (use-package eglot-booster
;;   :ensure (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (setq eglot-booster-io-only t)
;;   (eglot-booster-mode +1))

;; (use-package consult-eglot
;;   :ensure t
;;   :after (consult eglot)
;;   :bind (:map eglot-mode-map ("C-c l s" . consult-eglot-symbols))
;;   :config
;;   (setq consult-eglot-sort-results t))

;; (use-package consult-eglot-embark ;; 待研究
;;   :ensure t
;;   :after (consult eglot embark)
;;   :config
;;   (consult-eglot-embark-mode +1))

;; (use-package breadcrumb
;;   :ensure t
;;   :config (breadcrumb-mode +1))

;; (use-package eldoc
;;   :ensure (:type built-in)
;;   :config
;;   (setq eldoc-idle-delay 0)
;;   (setq eldoc-print-after-edit nil)
;;   (setq eldoc-echo-area-use-multiline-p nil)
;;   (setq eldoc-documentation-strategy #'eldoc-documentation-compose)
;;   (setq eldoc-echo-area-prefer-doc-buffer t)
;;   (setq eldoc-echo-area-display-truncation-message nil))

;; (use-package flymake
;;   :ensure (:type built-in)
;;   :init
;;   (defun kaladin/flymake-setup ()
;;     "Set up Flymake with buffer-local checking."
;;     ;; 将局部的 meow 钩子添加到每个 buffer
;;     (add-hook 'meow-insert-exit-hook #'kaladin/enable-flymake-timed-check nil t)
;;     (add-hook 'meow-insert-enter-hook #'kaladin/disable-flymake-timed-check nil t))

;;   ;; 在 normal 状态下进行 Flymake 检查
;;   (defun kaladin/enable-flymake-timed-check ()
;;     "Trigger Flymake check when entering normal state in buffer-local context."
;;     (setq flymake-no-changes-timeout 0.5)  ;; 设置检查间隔
;;     (flymake-start))                     ;; 立即检查

;;   ;; 在 insert 状态下禁用 Flymake 自动检查
;;   (defun kaladin/disable-flymake-timed-check ()
;;     "Disable Flymake timed check when entering insert state."
;;     (setq flymake-no-changes-timeout nil))
;;   :bind (:map flymake-mode-map
;;               ("M-n" . flymake-goto-next-error)
;;               ("M-p" . flymake-goto-prev-error))

;;   :config
;;   (setq flymake-no-changes-timeout nil)
;;   (setq flymake-mode-line-lighter " 🐞")
;;   ;; flymake-error-bitmap
;;   ;; flymake-warning-bitmap
;;   ;; flymake-fringe-indicator-position
;;   ;; flymake-wrap-around
;;   ;; warning-minimum-log-level
;;   ;; warning-minimum-level
;;   ;; flymake-mode-line-lighter

;;   ;; 将 kaladin/flymake-setup 添加到 flymake-mode 启动时的 hook 中
;;   (add-hook 'flymake-mode-hook #'kaladin/flymake-setup))

(provide 'core-language)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-language.el ends here
