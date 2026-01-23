;;; core-consult.el --- Core Consult -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :ensure t
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
         ("C-x C-b" . consult-project-buffer)         ; 使用 consult-buffer 替代默认的缓冲区切换命令

         ;; consult-goto-line     j
         ;; consult-mark          m
         ;; consult-global-mark   g
         ;; consult-outline       o
         ;; consult-imenu-multi   i
         ;; consult-line-multi    l
         ;; consult-keep-lines    d
         ;; consult-focus-lines   n
         ;; consult-fd            f
         ;; consult-org-agenda    a
         ;; consult-man           h
         ;; consult-info          .
         ;; consult-theme         t
         ;; consult-complex-command  c  ;; 重复复杂的命令


         ;; consult-history             ;; 终端或 minibuffer 等特殊区域 map 使用 可以由 cape-history 替代

         )
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
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)

  ;; 实时预览与编辑
  ;; consult-preview-allowed-hooks 预览时会禁用大多数 mode 配置此变量以设置白名单
  (setq consult-preview-key (list :debounce 0.5 'any))

  ;; 项目搜索
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; wgrep

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

;; (use-package embark
;;   :ensure t
;;   ;; :bind
;;   ;; (:map global-map
;;   ;;       ("C-<f14>" . embark-act)         ;; pick some comfortable binding
;;   ;;       ("<f14>" . embark-dwim))
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;
;;   ;; Show the Embark target at point via Eldoc. You may adjust the
;;   ;; Eldoc strategy, if you want to see the documentation from
;;   ;; multiple providers. Beware that using this can be a little
;;   ;; jarring since the message shown in the minibuffer can be more
;;   ;; than one line, causing the modeline to move up and down:
;;
;;   ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;;   ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
;;
;;   :config
;;   ;;   (defun embark-which-key-indicator ()
;;   ;;     "An embark indicator that displays keymaps using which-key.
;;   ;; The which-key help message will show the type and value of the
;;   ;; current target followed by an ellipsis if there are further
;;   ;; targets."
;;   ;;     (lambda (&optional keymap targets prefix)
;;   ;;       (if (null keymap)
;;   ;;           (which-key--hide-popup-ignore-command)
;;   ;;         (which-key--show-keymap
;;   ;;          (if (eq (plist-get (car targets) :type) 'embark-become)
;;   ;;              "Become"
;;   ;;            (format "Act on %s '%s'%s"
;;   ;;                    (plist-get (car targets) :type)
;;   ;;                    (embark--truncate-target (plist-get (car targets) :target))
;;   ;;                    (if (cdr targets) "…" "")))
;;   ;;          (if prefix
;;   ;;              (pcase (lookup-key keymap prefix 'accept-default)
;;   ;;                ((and (pred keymapp) km) km)
;;   ;;                (_ (key-binding prefix 'accept-default)))
;;   ;;            keymap)
;;   ;;          nil nil t (lambda (binding)
;;   ;;                      (not (string-suffix-p "-argument" (cdr binding))))))))
;;
;;   ;;   (setq embark-indicators
;;   ;;         '(embark-which-key-indicator
;;   ;;           embark-highlight-indicator
;;   ;;           embark-isearch-highlight-indicator))
;;
;;   ;;   (defun embark-hide-which-key-indicator (fn &rest args)
;;   ;;     "Hide the which-key indicator immediately when using the completing-read prompter."
;;   ;;     (which-key--hide-popup-ignore-command)
;;   ;;     (let ((embark-indicators
;;   ;;            (remq #'embark-which-key-indicator embark-indicators)))
;;   ;;       (apply fn args)))
;;
;;   ;;   (advice-add #'embark-completing-read-prompter
;;   ;;               :around #'embark-hide-which-key-indicator)
;;
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))



(provide 'core-consult)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-consult.el ends here
