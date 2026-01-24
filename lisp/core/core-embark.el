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
  (setq embark-confirm-act-all t)  ;; 熟悉后可设置为 nil 取消对每项都执行命令的询问
  ;; (setq embark-prompter 'embark-completing-read-prompter)


  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; 高亮当前目标以表明 embark 对其生效
;; embark-indicators

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



;;   (defun embark-which-key-indicator ()
;;     "An embark indicator that displays keymaps using which-key.
;; The which-key help message will show the type and value of the
;; current target followed by an ellipsis if there are further
;; targets."
;;     (lambda (&optional keymap targets prefix)
;;       (if (null keymap)
;;           (which-key--hide-popup-ignore-command)
;;         (which-key--show-keymap
;;          (if (eq (plist-get (car targets) :type) 'embark-become)
;;              "Become"
;;            (format "Act on %s '%s'%s"
;;                    (plist-get (car targets) :type)
;;                    (embark--truncate-target (plist-get (car targets) :target))
;;                    (if (cdr targets) "…" "")))
;;          (if prefix
;;              (pcase (lookup-key keymap prefix 'accept-default)
;;                ((and (pred keymapp) km) km)
;;                (_ (key-binding prefix 'accept-default)))
;;            keymap)
;;          nil nil t (lambda (binding)
;;                      (not (string-suffix-p "-argument" (cdr binding))))))))

;;   (setq embark-indicators
;;         '(embark-which-key-indicator
;;           embark-highlight-indicator
;;           embark-isearch-highlight-indicator))

;;   (defun embark-hide-which-key-indicator (fn &rest args)
;;     "Hide the which-key indicator immediately when using the completing-read prompter."
;;     (which-key--hide-popup-ignore-command)
;;     (let ((embark-indicators
;;            (remq #'embark-which-key-indicator embark-indicators)))
;;       (apply fn args)))

;;   (advice-add #'embark-completing-read-prompter
;;               :around #'embark-hide-which-key-indicator)



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
