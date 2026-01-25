;;; core-consult.el --- Core Consult -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rg
  :ensure t
  :bind (:map global-map
              ("C-c s" . rg-menu)))

(use-package wgrep
  :ensure t
  :bind((:map grep-mode-map
              ("C-x C-s" . wgrep-save-all-buffers)
              ("C-x C-q" . wgrep-change-to-wgrep-mode))
        (:map wgrep-mode-map
              ("C-c C-c" . wgrep-finish-edit)
              ("C-c C-k" . wgrep-abort-changes)

              ("C-c C-r" . wgrep-remove-change)      ;; 待定
              ("C-c C-u" . wgrep-remove-all-change)  ;; 待定

              ("C-x C-r" . wgrep-toggle-readonly-area)
              ("C-x C-d" . wgrep-mark-deletion)
              ("C-x C-s" . wgrep-finish-edit)
              ("C-x C-q" . wgrep-exit)))

  :config
  ;; To save all buffers that wgrep has changed, run
  ;; M-x wgrep-save-all-buffers
  (setq wgrep-auto-save-buffer nil)
  (setq wgrep-change-readonly-file nil))

(use-package consult
  :ensure t
  :init
  (defvar 1043/consult-map (make-sparse-keymap)
    "Keymap for `consult'.")
  (which-key-add-key-based-replacements "C-x c" "consult")
  :bind-keymap
  ("C-x c" . 1043/consult-map)
  :bind
  ((:map global-map
         ("C-r" . consult-ripgrep)

         ("M-s" . consult-line)             ; 使用 consult-line 替代默认的逐行搜索命令
         ("M-i" . consult-imenu)          ;; org-mode 时 consult-org-heading
         ("M-y" . consult-yank-from-kill-ring)

         ("C-x b" . consult-buffer)         ; 使用 consult-buffer 替代默认的缓冲区切换命令
         ("C-x r" . consult-register)
         ("C-x f" . consult-recent-file)
         ("C-x s" . consult-register-store)

         ("C-x C-k" . consult-kmacro) ;; 原快捷键功能 kmacro-keymap
         ("C-x C-b" . consult-project-buffer))         ; 使用 consult-buffer 替代默认的缓冲区切换命令
   (:map 1043/consult-map
         ("j" . consult-goto-line)
         ("m" . consult-mark)
         ("g" . consult-global-mark)
         ("o" . consult-outline)
         ("i" . consult-imenu-multi)
         ("l" . consult-line-multi)
         ("d" . consult-keep-lines)
         ("n" . consult-focus-lines)
         ("f" . consult-fd)
         ("a" . consult-org-agenda)
         ("h" . consult-man)
         ("." . consult-info)
         ("t" . consult-theme)
         ;; 重复复杂的命令
         ("c" . consult-complex-command))
   ;; consult-history             ;; 终端或 minibuffer 等特殊区域 map 使用 可以由 cape-history 替代
   ;; 可以将 consult-history 绑定到 minibuffer-local-map

   (:map isearch-mode-map
         ("M-e" . consult-isearch-history)))

  :config
  ;; consult-buffer-sources

  ;; consult-buffer-list-function
  ;; consult-project-buffer-sources

  ;; consult-buffer-filter

  ;; register
  (setq register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; xref
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  ;; 实时预览与编辑
  ;; consult-preview-allowed-hooks 预览时会禁用大多数 mode 配置此变量以设置白名单
  (setq consult-preview-key (list :debounce 0.5 'any))

  ;; 项目搜索
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))



  (consult-customize
   ;; 完全不预览主题
   consult-theme :preview-key nil
   ;; 项目内总是预览
   consult-project-buffer :preview-key 'any)

  ;; Backspace 就可以用于 widen
  (setq consult-widen-key nil)
  ;; 切换 buffer 过滤条件
  (setq consult-narrow-key "=")
  ;; 输入字符的最小数量
  (setq consult-async-min-input 2)
  ;; 保留原始的字体属性
  (setq consult-fontify-preserve t)
  ;; 最多同时预览缓冲区数量
  (setq consult-preview-max-count 5)
  ;; 限制显示匹配行的最大列数
  (setq consult-grep-max-columns 99)
  ;; 跳转行时 minibuffer 显示行号
  (setq consult-goto-line-numbers t)
  ;; 当缩放功能激活时显示绝对行号
  (setq consult-line-numbers-widen t)
  ;; 总是从头开始检索行
  (setq consult-line-start-from-top t)
  ;; 异步命令的刷新延迟
  (setq consult-async-refresh-delay 0.2)
  ;; 异步命令的输入节流 每 0.5s 最多执行一次命令
  (setq consult-async-input-throttle 0.5)
  ;; 异步命令的输入防抖 输入停顿 0.3s 后才执行命令
  (setq consult-async-input-debounce 0.3)
  ;; 避免对过大的缓冲区进行字体化
  (setq consult-fontify-max-size 1048576)
  ;; 对过大的文件进行分块读取
  (setq consult-preview-partial-chunk 10240)
  ;; 对过大的文件进行部分预览
  (setq consult-preview-partial-size 1048576)
  ;; 跳转至匹配项时，总位于匹配项开头
  (setq consult-point-placement 'match-beginning))

(provide 'core-consult)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-consult.el ends here
