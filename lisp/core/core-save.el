;;; core-save.el --- Core-Save -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (history-length 300)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     compile-command                  ; 编译命令历史
     file-name-history                ; 文件名历史
     minibuffer-history               ; minibuffer 历史
     query-replace-history            ; 查找替换历史
     shell-command-history            ; shell 命令历史
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring))); 搜索记录

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 600))

(use-package super-save
  :ensure t
  :delight
  :config
  ;; 不对远程文件自动保存（scp/ssh、TRAMP）
  (setq super-save-remote-files nil)
  ;; 不对加密文件（.gpg/.asc 等）自动保存，避免解密/覆盖问题
  ;; 使用正则更稳健，匹配文件名后缀
  (setq super-save-exclude '("\.gpg\'" "\.asc\'"))
  ;; 静默模式：不显示保存提示
  (setq super-save-silent t)
  ;; 保存前删除行末空白（谨慎：可能改变历史）
  (setq super-save-delete-trailing-whitespace t)
  (setq super-save-auto-save-when-idle nil)

  (setq super-save-all-buffers nil) ;; 待考虑

  ;; 触发器：把 ace-window 加入（你已配置），以及常见的窗口切换命令
  (dolist (cmd '(ace-window other-window
                            switch-to-buffer windmove-left windmove-right
                            windmove-up windmove-down))
    (add-to-list 'super-save-triggers cmd))

  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)

  ;; 启动 super-save 模式
  (super-save-mode +1))

(provide 'core-save)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-save.el ends here
