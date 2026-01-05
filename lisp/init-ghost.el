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
  (setq tab-always-indent 'complete) ; 按 tab 时尝试缩进，若缩进无变化则尝试补全 init.el
  (setq tab-first-completion 'word-or-paren-or-punct) ;; init.el

  ;; minibuffer
  ;; 允许在 minibuffer 中递归打开 minibuffer（复杂交互有用）
  (setq-default enable-recursive-minibuffers t))
  
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
