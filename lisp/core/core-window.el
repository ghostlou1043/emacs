;;; core-window.el --- Core Window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package desktop
  :ensure nil
  :if (1043/enable-desktop-p)
  :init
  ;; 启动时，只立即恢复前 5 个 buffer 的内容。
  (setq desktop-restore-eager 5)
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'1043/desktop-setup)
    (add-hook 'after-init-hook #'1043/desktop-setup))
  :config
  ;; 当 Emacs 在后台“懒加载”剩余文件时，不要在 minibuffer 显示烦人的消息
  (setq desktop-lazy-verbose nil)

  ;; 恢复 frames, 若为 nil 则仅保存 buffer
  ;; 似乎对 daemon 没有破坏性影响，且非 daemon 下可以用于恢复布局
  ;; 但 daemon 下保存的布局可以能被应用到非 daemon 下的 Emacs 中
  ;; 通过使用 eyebrowse-restore 或 activities 来恢复布局
  ;; 经测试 eyebrowse-restore 依赖 desktop-restore-frames
  (setq desktop-restore-frames t)

  ;; (setq desktop-dirname) ;; 保持默认
  ;; (setq desktop-base-file-name) ;; 保持默认
  ;; (setq desktop-buffers-not-to-save) ;; 暂时保持默认

  (setq desktop-auto-save-timeout 90)
  ;; 不询问直接保存，当临时使用 Emacs 时会造成污染，待配置
  ;; 使用 easysession 平替 desktop.el
  (setq desktop-save t))

(use-package winner
  :ensure nil
  :unless (1043/enable-tab-bar-p)
  :hook (after-init . winner-mode)
  :bind
  (:map global-map
        ("C-x w u" . winner-undo)
        ("C-x w r " . winner-redo)))

(use-package tab-bar
  :ensure nil
  :hook ((after-init . tab-bar-mode)
         (after-init . tab-bar-history-mode))
  :bind  ;; 如果使用 tab-bar 则必绑定 activities 部分命令交由其实现
  (:map global-map
        ("C-x w u" . tab-bar-history-back)
        ("C-x w r" . tab-bar-history-forward)
        ("C-x w t" . tab-bar-switch-to-tab)
        ("C-x w b" . tab-bar-switch-to-prev-tab)
        ("C-x w f" . tab-bar-switch-to-next-tab))

  :config
  (setq tab-bar-show t)                  ;; 设为 1 时则 tab 小于 1 个时自动隐藏
  (setq tab-bar-truncate t)              ;; 截断 tab-bar 仅显示一行

  ;; 待定
  (setq tab-bar-auto-width nil)
  (setq tab-bar-new-tab-group nil)  ; 不自动分组


  ;; tab-bar-new-tab-group ;; tab-bar 的组有什么用？有必要用吗？
  ;; (setq tab-bar-define-keys t)

  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)

  (setq tab-bar-tab-hints t)            ;; 显示 Tab 的数字
  (setq tab-bar-separator "")
  (setq tab-bar-new-button nil)
  (setq tab-bar-back-button nil)
  (setq tab-bar-close-button nil)
  (setq tab-bar-forward-button nil)
  (setq tab-bar-menu-bar-button nil)
  ;; (setq tab-bar-select-tab-modifiers '(meta)
  
  ;; 不知是否会与 activities 冲突
  (setq tab-bar-new-tab-choice "*scratch*")

  ;; 截断长名
  (setq tab-bar-tab-name-truncated-max 20)
  (setq tab-bar-tab-name-function #'tab-bar-tab-name-truncated)
  
  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let* ((face (funcall tab-bar-tab-face-function tab))
                 (current-p (eq (car tab) 'current-tab))  ; 判断是否为当前 tab
                 (number-face (if current-p
                                  `(:inherit ,face :weight ultra-bold :underline t)
                                `(:inherit ,face :weight ultra-bold))))  ; 非活动 tab 无下划线
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face number-face)
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; 把 meow 的 indicator 也放在 tab-bar 上
  (setq tab-bar-format '(meow-indicator
                         tab-bar-format-tabs
                         tab-bar-format-align-right ;; 这是一个特殊的“占位符”，让后面的东西都跑到最右边
                         tab-bar-format-global    ;; global-mode-string 都可以显示些什么东西呢？
                         )))

;; 全屏下才考虑显示时间等
;; (setq display-time-format " %H:%M ")

;; 如果发现在 daemon 下创建 frame 后 tab-bar 刷新不及时，使用其搭配 hook 强制刷新
;; (tab-bar--update-tab-bar-lines)

(use-package activities
  :ensure t
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  ;; (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  (defun 1043/rename-initial-scratch-tab ()
    "如果当前只有一个 tab，并且它叫 *scratch*，就重命名为 Misc。"
    (interactive)
    (when (and (bound-and-true-p tab-bar-mode)
               (= (length (tab-bar-tabs)) 1)  ; 只有 1 个 tab
               (string= (alist-get 'name (tab-bar--current-tab)) "*scratch*"))
      (tab-bar-rename-tab "Misc")))
  ;; (message "✓ 初始 tab 已重命名为 'Misc'")))
  
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'1043/rename-initial-scratch-tab)
    (add-hook 'emacs-startup-hook #'1043/rename-initial-scratch-tab))
  
  :bind
  (("C-x w n" . activities-new)
   ("C-x w d" . activities-define)
   ("C-x w c" . activities-resume)
   ("C-x w z" . activities-suspend)
   ("C-x w k" . activities-kill)
   ("C-x w w" . activities-switch)
   ("C-x w s" . activities-switch-buffer)
   ("C-x w g" . activities-revert)
   ("C-x w l" . activities-list))

  :config
  (setq activities-name-prefix "")
  (setq activities-always-persist t) ;; 设为 nil 则仅在退出时保存，t 时则在保存 buffer 时也保存
  (setq activities-bookmark-store t)
  (setq activities-set-frame-name t) ;; Only applies when activities-tabs-mode is disabled.

  (setq activities-resume-into-frame 'current) ;; resume 时使用当前 frame
  (setq activities-mode-idle-frequency 60)     ;; 闲置 60s 后保存 activities
  (setq activities-bookmark-warnings t) ;; 不能存为 bookmark 时警告

  (setq tab-bar-tab-face-function 'tab-bar-tab-face-default) ;; 令 tab-bar 不被 activities 影响

  ;; 该变量的颜色体现在 activities-resume 界面，而非 tab-bar
  (setq activities-annotation-colors '("blue" "red" 0.65))

  ;; activities-default-name-fn
  ;; activities-kill-buffers
  ;; activities-tabs-mode-hook

  ;; activities-anti-kill-predicates
  ;; activities-anti-save-predicates
  ;; activities-tabs-tab-bar-tab-face-function-original
  ;; activities-tabs-before-resume-functions

  )




;; pk 与 popper 考虑

;; help-window-select 自动焦点 help 窗口

;; 考虑 eyebrowse+burly 进行布局切换 + 布局恢复
;; 考虑 popper+shackle 进行窗口弹出 + 弹出方式控制
;; 暂不考虑 desktop-save-mode 来持久化布局？ 除非 window 很多再考虑？
;; 暂不考虑 zoom 来自动调整当前 window 大小 (可能头晕或影响观感) 不过用来弹 help 似乎也不错？

(use-package shackle
  :ensure t
  :config
  (setq shackle-select-reused-windows t)  ; 光标依旧焦点已有窗口
  (setq help-window-select t) ;; 如果想要光标不聚集 *help* 需要将该变量设置为 nil

  ;; 符号匹配的缓冲区的主要模式
  ;; 字符串匹配缓冲区的名称
  ;; 通过在键值部分使用键 :regexp t 将其转换为正则表达式匹配

  (setq shackle-rules ;; 待定制
        '(
          ;; Right
          (helpful-mode :align right :size 0.5)
          ;; ("^\\*helpful.*" :regexp t :select t :align right :size 0.5)

          ("*Help*" :align right :size 0.5)
          ("*eldoc*" :align right :size 0.5)
          ("*gt-result*" :align right :size 0.5)

          ;; Below
          ("*Messages*" :align below :size 0.3)
          ("*compilation*" :align below :size 0.3)
          ("*Warnings*" :align below :size 0.3)

          ;; Shell
          ("*shell*" :align below :size 0.3 :popup t)
          ("*eshell*" :align below :size 0.3 :popup t)
          ("*term*" :align below :size 0.3 :popup t)
          ("*vterm*" :align below :size 0.3 :popup t) ;; 必须要 popup t 才有效
          ("*eat*" :align below :size 0.3 :popup t)
          ))

  ;; 匹配未在 shackle-rules 中匹配到的 buffer
  ;; 暂时不配置保持默认
  ;; (setq shackle-default-rule
  ;;       '(:select t))

  (shackle-mode +1))

(use-package popper
  :ensure t
  :after (shackle)
  :bind
  (:map global-map
        ("C-z" . popper-toggle) ;; EAT 吞 C-z , 利用 meow 的 q 键退出，配置稳定后尝试修复
        ("M-z" . popper-toggle-type)
        ;; 使用 popper-kill-latest-popup 关闭已打开的弹窗缓冲区
        ("C-x w p" . popper-cycle)
        ("C-x w q" . popper-kill-latest-popup))
  :config
  (setq popper-display-control nil) ;; 使用 Shackle 控制窗口的弹出
  (setq popper-group-function #'popper-group-by-projectile) ;; 按照项目分组弹窗
  ;; (setq popper-mode-line nil) ;; 禁止 mode-line 显示 pop
  ;; (setq popper-echo-dispatch-keys nil) ;; 不建议使用按键跳转 至少不要是字母
  (setq popper-echo-dispatch-keys '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

  ;; echo 的显示行数及截断
  (setq popper-echo-lines 1)  ; 设置为 2 行显示
  (setq popper-echo-transform-function
        (lambda (name)
          (if (> (length name) 10)
              (concat (substring name 0 10) "…") ; 截断长名称
            name)))  ; 保留短名称


  (setq popper-reference-buffers
        '("\\*Messages\\*"                 ; 匹配 *Messages* buffer
          ;; ("Output\\*$" . hide)           ; 匹配以 Output* 结尾的 buffer, 并且抑制了弹窗，后续也可正常调出
          "Output\\*$"           ; 匹配以 Output* 结尾的 buffer
          "\\*gt-result\\*"
          "\\*Help\\*"
          help-mode
          "^\\*helpful.*\\*$"
          helpful-mode
          "\\*eldoc\\*"
          compilation-mode                 ; 所有 compilation-mode 的 buffers
          ))

  ;; Match eat, eshell, shell, term and/or vterm buffers
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eat.*\\*$"    eat-mode
                  "^\\*term.*\\*$"   term-mode   ;term as a popup
                  "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                  "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                  "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                  )))

  ;; Async Shell Command 存在输出时才弹窗
  ;; (defun popper-shell-output-empty-p (buf)
  ;;   (and (string-match-p "\\*Async Shell Command\\*" (buffer-name buf))
  ;;        (= (buffer-size buf) 0)))
  ;;
  ;; (add-to-list 'popper-reference-buffers
  ;;              '(popper-shell-output-empty-p . hide))

  (popper-mode +1)
  (popper-echo-mode +1))

;; (use-package eyebrowse
;;   :ensure t
;;   :hook (elpaca-after-init . eyebrowse-mode)
;;   :init
;;   (setq eyebrowse-keymap-prefix (kbd "C-x w"))
;;   :bind (:map eyebrowse-mode-map
;;               ("C-x w b" . eyebrowse-switch-to-window-config)
;;               ("C-x w b" . eyebrowse-prev-window-config)
;;               ("C-x w f" . eyebrowse-next-window-config)
;;               ("C-x w l" . eyebrowse-last-window-config)
;;
;;               ("C-x w n" . eyebrowse-rename-window-config)
;;               ("C-x w c" . eyebrowse-close-window-config)
;;               ("C-x w w" . eyebrowse-create-window-config)
;;               )
;;   :config
;;   ;; 使 eyebrowse 的布局切换对 treemacs 等也生效
;;   (add-to-list 'window-persistent-parameters '(window-side . writable))
;;   (add-to-list 'window-persistent-parameters '(window-slot . writable))
;;
;;   ;; (frame-parameter nil 'name)
;;   ;; (set-frame-parameter nil 'name "Main")
;;
;;   ;; 不使用 eyebrowse-setup-opinionated-keys , 避免 M-0~9 被占用
;;   (eyebrowse-mode +1))

;; (use-package eyebrowse-restore
;;   :ensure t
;;   :bind
;;   ;; eyebrowse-restore-save-all
;;   :init
;;   (if (boundp 'elpaca-after-init-hook)
;;       (add-hook 'elpaca-after-init-hook #'eyebrowse-restore-mode)
;;     (add-hook 'after-init-hook #'eyebrowse-restore-mode))
;;   :config
;;   (setq eyebrowse-restore-save-interval 300))




(provide 'core-window)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-window.el ends here
