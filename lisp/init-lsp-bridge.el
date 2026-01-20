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
  :config
  (setq lsp-bridge-python-command
        (expand-file-name "~/.config/emacs/lsp-bridge/.venv/bin/python"))
  (setq acm-candidate-match-function 'orderless-flex)
  
  (setq lsp-bridge-python-lsp-server "ty")
  (setq lsp-bridge-nix-lsp-server "nixd")
  
  (setq lsp-bridge-org-babel-lang-list nil)
  (setq lsp-bridge-enable-diagnostics t)
  (setq lsp-bridge-enable-hover-diagnostic nil) ;; 光标移动到错误位置弹出诊断信息， 默认关闭
  (setq lsp-bridge-diagnostic-fetch-idle 0.5)
  (setq lsp-bridge-enable-signature-help t) ;; 支持函数参数显示， 默认打开
  (setq lsp-bridge-disable-backup t)
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-completion-in-string-file-types '("vue" "dart" "html"))

  (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
  (setq lsp-bridge-signature-show-with-frame-position "bottom-right")

  (setq lsp-bridge-enable-org-babel t)
  
  ;; acm-completion-mode-candidates-merge-order
  ;; acm-backend-order
  ;; acm-enable-capf
  
  (setq acm-enable-doc t)
  (setq acm-enable-icon t)
  (setq acm-enable-doc-markdown-render 'async)
  (setq acm-backend-search-sdcv-words-dictionary
        (expand-file-name "~/.stardict/dic/stardict-langdao-ec-gb-2.4.2/langdao-ec-gb"))

  (setq acm-backend-lsp-match-mode "fuzzy")

  ;; lsp-bridge-enable-inlay-hint: 类型嵌入提示， 默认关闭， 这个选项对于那些严重依赖类型提示的语言比较有用， 比如 Rust
  ;; lsp-bridge-enable-search-words: 索引打开文件的单词， 默认打开
  ;; lsp-bridge-enable-auto-format-code: 自动格式化代码, 默认关闭
  ;; lsp-bridge-enable-document-highlight: 高亮文档中相同的符号， 默认关闭
  ;; lsp-bridge-log-level: 设置 LSP 消息日志等级， 默认为 'default, 除非开发目的， 平常请勿将此选项设置成debug, 以避免影响性能

  ;; lsp-bridge-default-mode-hooks

  ;; lsp-bridge-remote-heartbeat-interval
  ;; lsp-bridge 除了提供 LSP 补全以外， 也提供了很多非 LSP 的补全后端，
  ;; 包括 capf、 文件单词、 路径、 Yas/Tempel、 TabNine、 Codeium、 Copilot、 Tabby, Citre、 Ctags, Org roam 等补全后端，
  ;; 如果你期望在某个模式提供这些补全， 请把对应的模式添加到 lsp-bridge-default-mode-hooks, 定义补全顺序请查看 acm-backend-order

  ;; 可以通过自定义 lsp-bridge-get-project-path-by-filepath 函数来告诉 lsp-bridge 项目的根目录， 这个函数输入参数是打开文件的路径字符串， 输出参数是项目目录路径

  (global-lsp-bridge-mode))

(provide 'init-lsp-bridge)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-lsp-bridge.el ends here
