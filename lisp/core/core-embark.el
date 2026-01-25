;;; core-embark.el --- Core Embark -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package embark
  :ensure t
  :bind
  (:map global-map
        ("C-'" . embark-act)         ;; pick some comfortable binding
        ("M-'" . embark-dwim))       ;; embark-dwim ，它会运行找到的第一个目标的默认操作，在非 minibuffer 缓冲区中非常方便
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; 熟悉后可设置为 nil 取消对每项都执行命令的询问
  (setq embark-confirm-act-all t)
  ;; 将 @ 快选改为 \ 快选，该项占据 `C-x \' `C-c \' `C-x C-\' `C-c C-\' `C-M-\' 及各种 前缀+\ 的快捷键
  (setq embark-keymap-prompter-key "\\")
  ;; (setq embark-prompter 'embark-completing-read-prompter)

  ;; 指定默认情况下操作不应退出 minibuffer，但使用 kill-buffer 作为操作时应退出
  (setq embark-quit-after-action
        '((kill-buffer . t)
          (t . nil)))

  ;; 也可以为不退出的 emabrk-act 单独定制一个函数
  ;; (defun embark-act-noquit ()
  ;;   "Run action but don't quit the minibuffer afterwards."
  ;;   (interactive)
  ;;   (let ((embark-quit-after-action nil))
  ;;     (embark-act)))

  ;; 高亮当前目标以表明 embark 对其生效
  (setq embark-indicators
        '(embark--vertico-indicator
          embark-mixed-indicator
          ;; embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (setq embark-mixed-indicator-delay 0.5)
  
  ;; keycast 
  (with-eval-after-load 'keycast
    (defun store-action-key+cmd (cmd)
      (force-mode-line-update t)
      (setq this-command cmd
            keycast--this-command-keys (this-single-command-keys)
            keycast--this-command-desc cmd))
    (advice-add 'embark-keymap-prompter :filter-return #'store-action-key+cmd)
    ;; version of keycast--update that accepts (and ignores) parameters
    (defun force-keycast-update (&rest _) (keycast--update))
    (advice-add 'embark-act :before #'force-keycast-update))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; 模式行会显示选中目标的计数
;; embark-selection-indicator

;; 为某一类型提供哪些操作
;; embark-keymap-alist

;; 生成一个缓冲区，列出所有当前候选物，供你随意浏览并在其中执行操作。
;; 候选物以显示附加注释的列表形式展示。如果任何候选物包含换行符，则使用水平线来分隔候选物
;; embark-collect
;; embark-live 实时更新的 embark-collect , 应用场景通常是从普通缓冲区调用，以显示缓冲区的某种实时更新“目录”

;; embark-select
;; embark-export ;; 当在导出和收集之间犹豫不决时，一个不错的经验法则是始终优先选择 embark-export 它能够回退到 embark-collect
;; embark-general-map 用于绑定始终都可用的操作

;; embark-become 只在 minibuffer 中工作
;; embark-become 中途改变想要执行的命令，如 switch-to-buffer 发现没有 buffer 转而使用 find-file
;; 可以将 embark-become 绑定到 minibuffer-local-map
;; embark-become-keymaps ;; 最终 C-. B f 来切换到 find-file

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))



(provide 'core-embark)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-embark.el ends here
