;;; init-ghost.el --- Init-Ghost -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :init
  ;; GPG
  (setq epa-file-select-keys 0)
  (setq epg-pinentry-mode 'loopback) ; 让 Emacs 内置 pinentry（GPG PIN）以 loopback 模式处理（在 Emacs 中输入密码）
  ;; (setq epa-file-cache-passphrase-for-symmetric-encryption nil) ; 同一个会话期间缓存密码

  ;; 可能需要调整
  (setq-default fringes-outside-margins nil) ; 让 fringes 在 margins 内部（影响 diff-hl、flymake 等显示）
  (setq confirm-kill-processes nil) ;; 设为 nil 使退出自动杀掉进程
  (setq y-or-n-p-use-read-key t)
  (setq-default indicate-buffer-boundaries nil) ;; 在fringe提示缓冲区边界

  ;; Git
  (setq vc-handled-backends '(Git)) ; 只启用 Git 作为版本控制后端（避免其他后端性能/噪音）

  ;; Message
  (setq suggest-key-bindings nil)  ;; 取消命令的快捷键提示，避免干扰以及 message 的重复输出

  ;; Visual
  (setq truncate-lines t) ; 默认不自动折行（长行在一行显示，右侧截断） init.el

  ;; 粘贴
  (setq kill-ring-max 200) ; kill-ring（剪切历史）最大保存 200 项
  (setq kill-do-not-save-duplicates t) ; 不保存重复的 kill 条目（减少冗余） init.el
  (setq-default mouse-yank-at-point nil) ; 鼠标粘贴时在光标处插入，而不是在点击位置 init.el
  (setq-default select-enable-clipboard t) ; 使用系统剪贴板（和外部程序共享剪贴板）
  (setq-default select-enable-primary nil) ; 禁用 X Window PRIMARY 选择（避免意外粘贴）

  ;; 缩进
  (setq indent-tabs-mode nil) ; 取消使用 tab 字符，改用空格缩进 init.el
  (setq tab-width 4) ; 将 tab 显示宽度设置为 4（仅展示，与实际插入空格无关） init.el

  ;; 补全
  (setq completion-cycle-threshold nil)
  (setq tab-always-indent 'complete) ; 按 tab 时尝试缩进，若缩进无变化则尝试补全 init.el
  ;; (setq tab-first-completion 'word-or-paren-or-punct) ;; init.el
  (setq tab-first-completion nil) ;; 如上配置会导致光标在括号前需按 2 次 TAB 键才弹出 corfu


  (setq text-mode-ispell-word-completion nil)
  (setq read-extended-command-predicate #'command-completion-default-include-p) ;; init.el

  ;; minibuffer
  (setq minibuffer-prompt-properties
        '(read-only t intangible t cursor-intangible t face minibuffer-prompt))  ;; init.el
  (setq-default enable-recursive-minibuffers t)   ;; 允许在 minibuffer 中递归打开 minibuffer（复杂交互有用）

  ;; 超大文件流畅编辑
  (setq-default bidi-display-reordering nil)
  (setq-default bidi-paragraph-direction 'left-to-right) ;; early-init.el

  ;; 进一步优化长行显示的性能阈值
  (setq bidi-inhibit-bpa t)              ; 禁止双向括号算法，进一步减少计算 ;; early-init.el
  (setq long-line-threshold 1000)          ; 行长超过1000字时，自动降低渲染精度以提升速度
  (setq large-hscroll-threshold 1000)      ; 超过1000字时，简化水平滚动逻辑
  (setq syntax-wholeline-max 1000)        ; 超过1000字时，限制语法高亮的扫描范围

  ;; 启用像素滚动
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode)) ; 启用更平滑的像素精度滚动模式（如果可用）


  ;; mode-line
  ;; (setq mode-line-collapse-minor-modes-to " …")
  (setq mode-line-collapse-minor-modes-to "")
  (setq mode-line-collapse-minor-modes
        '(apheleia-mode
          yas-minor-mode
          org-indent-mode
          easysession-save-mode
          ))

  ;; 行号
  (setq display-line-numbers-type 'relative)

  :hook ((emacs-lisp-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode)
         (prog-mode . display-line-numbers-mode))
  )

;; :bind
;; (:map global-map
;;       ("C-t" . nil) ; 取消 C-t 的默认绑定（transpose-chars）
;;       ("C-r" . nil) ; 取消 C-r 的默认绑定（isearch-backward）
;;       ("M-l" . move-to-window-line-top-bottom) ; M-l：循环在窗口顶部/底部移动（move-to-window-line-top-bottom）
;;       ("M-r" . repeat) ; M-r 绑定到 repeate
;;       ("C-x C-l" . delete-blank-lines) ; C-x C-l 绑定删除空行
;;       ("C-x C-o" . find-alternate-file) ; C-x C-o 绑定为 find-alternate-file（替换当前 buffer 文件）
;;       ("C-x C-v" . read-only-mode) ; 将 C-x C-v 绑定为 read-only-mode（覆盖默认 find-file）
;;       )





(provide 'init-ghost)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-ghost.el ends here
