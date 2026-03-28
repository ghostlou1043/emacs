;;; core-window.el --- Core Window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package winner
  :ensure nil
  :unless (1043/enable-tab-bar-p)
  :hook (after-init . winner-mode)
  :bind
  (:map global-map
        ("C-x w b" . winner-undo)
        ("C-x w f " . winner-redo)))

(use-package tab-bar
  :ensure nil
  :init
  (tab-bar-mode +1)
  (tab-bar-history-mode +1)

  :bind
  ((:map global-map
         ("C-x w b" . tab-bar-history-back)
         ("C-x w f" . tab-bar-history-forward)
         ("C-x w t" . tab-bar-switch-to-tab)
         ("C-x w p" . tab-bar-switch-to-prev-tab)
         ("C-x w n" . tab-bar-switch-to-next-tab))

   (:repeat-map tab-bar-repeat-map
                ("b" . tab-bar-history-back)
                ("f" . tab-bar-history-forward)
                ("p" . tab-bar-switch-to-prev-tab)
                ("n" . tab-bar-switch-to-next-tab)))

  :config
  (setq tab-bar-show t)                  ;; 设为 1 时则 tab 小于 1 个时自动隐藏
  (setq tab-bar-truncate t)              ;; 截断 tab-bar 仅显示一行

  (setq tab-bar-auto-width nil)
  (setq tab-bar-new-tab-group nil)  ; 不自动分组
  ;; tab-bar-new-tab-group ;; tab-bar 的组有什么用？有必要用吗？


  (setq tab-bar-define-keys nil)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-select-tab-modifiers nil)

  (setq tab-bar-tab-hints t)              ;; 显示 tab 序号，由于自定义该选项无效果
  (setq tab-bar-separator "")

  ;; 截断长名
  (setq tab-bar-tab-name-truncated-max 20)
  (setq tab-bar-tab-name-function #'tab-bar-tab-name-truncated)

  ;; 给 tab 两边加上空格，并加粗序号，当前 tab 序号下添加下划线
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

  (setq tab-bar-format '(tab-bar-format-tabs        ;; 标签页
                         tab-bar-format-align-right ;; 这是一个特殊的“占位符”，让后面的东西都跑到最右边
                         tab-bar-format-global      ;; 显示各种全局信息
                         )))

(use-package desktop
  :ensure nil
  :demand t
  :if (1043/enable-desktop-p)
  :custom
  ;; 退出时不询问直接保存
  (desktop-save t)
  ;; 启动时，只立即恢复前 5 个 buffer 的内容。
  (desktop-restore-eager 5)
  ;; 当 emacs 在后台“懒加载”剩余文件时，不要在 minibuffer 显示烦人的消息
  (desktop-lazy-verbose nil)
  ;; 保存并恢复布局
  (desktop-restore-frames t)
  ;; 允许加载被锁定的会话
  ;; (desktop-load-locked-desktop 't)
  (desktop-load-locked-desktop 'check-pid)

  :config
  ;; (desktop-save desktop-dirname)
  ;; (desktop-change-dir desktop-dirname)
  
  ;; (setq desktop-base-file-name ".emacs.desktop") ;; 保持默认
  ;; (setq desktop-dirname (expand-file-name user-emacs-directory)) ;; 保持默认
  ;; (setq desktop-buffers-not-to-save) ;; 暂时保持默认

  ;; desktop-read 恢复布局，但不允许重载
  ;; 而 desktop-change-dir 在 desktop-save-mode 开启的情况下会先保存当前 .emacs.desktop 文件再加载指定目录的 .emacs.desktop
  ;; 故此需关闭 desktop-save-mode 再利用 desktop-change-dir 重载并恢复布局

  (defun lou/desktop-save-guard (original-function &rest args)
    "只允许在 TUI 环境下保存 desktop，阻止 GUI 环境下的保存"
    (if (display-graphic-p)
        nil
      (apply original-function args)))

  (advice-add 'desktop-save :around #'lou/desktop-save-guard)

  (defun lou/desktop-change-dir-guard (original-function &rest args)
    "在执行 desktop-change-dir 期间，强制禁止保存桌面。"
    (let ((desktop-save nil))     
      (apply original-function args)))

  (advice-add 'desktop-change-dir :around #'lou/desktop-change-dir-guard)

  ;; 仅在 TUI 下恢复
  ;; 仅在 TUI 窗口(存在初始帧)为 1 时恢复
  ;; 仅在打开 buffer 为 *scratch* 或 *dashboard* 时恢复
  (add-hook 'server-after-make-frame-hook #'lou/restore-session)
  
  ;; 仅在最后一个 TUI frame 被关闭时保存
  (add-hook 'delete-frame-functions #'lou/desktop-save)

  ;; 空闲 300 秒自动保存一次
  ;; 手动保存时更新非 lock 存储文件，不更新 lock 文件
  ;; kill-emacs 时不保存, daemon 下关闭 frame 也不触发保存
  (setq desktop-auto-save-timeout 300)

  ;; 保留 desktop-save-mode 的超时保存能力，但不自动恢复，也不在切换前保存
  (add-hook 'emacs-startup-hook #'desktop-save-mode))

(use-package easysession ;; 说明：仅允许同时激活一个会话，会恢复多个 frame (包括 daemon 模式)与 frame 的布局以及所有 Buffer
  :ensure t
  ;; :ensure (easysession
  ;;          :fetcher github
  ;;          :repo "jamescherti/easysession.el"
  ;;          :branch "develop"  ;; 👈 关键：指定使用 develop 分支
  ;;          :files (:defaults "extensions/easysession*.el"))

  :demand t
  :if (1043/enable-easysession-p)
  :bind (:map global-map
              ;; 加载 Emacs 编辑会话，只恢复会话内容,不改变 frame 大小和位置, 适合切换 session 时使用
              ("C-x w =" . easysession-switch-to)
              ("C-x w L" . easysession-switch-to-and-restore-geometry)
              ("C-x w S" . easysession-save)
              ("C-x w C" . easysession-reset)
              ("C-x w R" . easysession-rename)
              ("C-x w U" . easysession-unload)
              ("C-x w D" . easysession-delete))

  :config
  ;; 后续考虑实现区分 TUI 与 GUI 不同会话，是否需要启动不同的 daemon ?

  ;; This extension makes EasySession persist and restore the scratch buffer.
  (with-eval-after-load 'easysession
    (require 'easysession-scratch)
    (easysession-scratch-mode +1))

  ;; This extension enables EasySession to persist and restore Magit buffers.
  (with-eval-after-load 'easysession
    (require 'easysession-magit)
    (easysession-magit-mode +1))

  ;; 通过 savehist 保存当前的 session name 并在下次启动时恢复对应的 session
  ;; The easysession package can leverage savehist save the restore the current session name
  (add-to-list 'savehist-additional-variables 'easysession--current-session-name)

  ;; 为 tab-bar 添加 [Easysession:main]
  (add-to-list 'global-mode-string '(:eval (easysession--mode-line-session-name-format)) 'append)

  (setq easysession-save-mode-lighter-show-session-name nil) ;; 显示在 mode 旁边
  (setq easysession-mode-line-misc-info nil)                 ;; 额外创建一个

  ;; easysession-exclude-from-find-file-hook

  ;; 仅保存当前可见的 buffer , 应该能加快 Emacs 的加载
  ;; (setq easysession-buffer-list-function 'easysession-visible-buffer-list)

  ;; How to create an empty session setup 暂时不需要

  ;; easysession-save-mode
  ;; Save every 10 minutes
  (setq easysession-save-interval 600)
  ;; 切换 session 前保存当前 session
  (setq easysession-switch-to-save-session t)

  ;; 仅在 GUI 下 使用 easysession 保存，也仅保存 GUI frame
  (setq easysession-save-mode-predicate #'display-graphic-p)

  ;; 仅在 GUI 下自动加载会话
  ;; (setq easysession-setup-load-predicate #'display-graphic-p)
  (setq easysession-setup-load-predicate #'(lambda()
                                             (display-graphic-p)))

  ;; 设置加载优先级
  (setq easysession-setup-add-hook-depth 102)
  ;; 为不同模式下启动的 Emacs 添加 hook
  (easysession-setup))

(use-package activities
  :ensure t
  :after tab-bar
  :init
  (defun 1043/rename-initial-tab ()
    "重命名初始 tab 为 Misc（支持多个默认名称）。"
    (when (bound-and-true-p tab-bar-mode)
      (let ((tabs (tab-bar-tabs)))
        (when (= (length tabs) 1)
          (let ((tab-name (alist-get 'name (car tabs)))
                ;; 定义要重命名的 tab 名字列表
                (target-names '("*scratch*" "*dashboard*")))
            ;; 如果当前名字在列表里，就重命名
            (when (member tab-name target-names)
              (tab-bar-rename-tab "Misc")))))))

  ;; daemon 下创建新 frame 重命名 (包括第一个 frame)
  ;; server 下创建新 frame 重命名 (不包括第一个 frame)
  (add-hook 'server-after-make-frame-hook #'1043/rename-initial-tab)

  ;; 为 server 和 非 daemon 和 server 模式下的初始 frame 重命名
  (unless (daemonp)
    (add-hook 'window-setup-hook #'1043/rename-initial-tab)
    ;; 为非 daemon 和 server 模式下的 frame 重命名
    (unless (server-running-p)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (1043/rename-initial-tab))))))

  ;; after-make-frame-functions 调用函数时，frame 已经创建但可能还没准备好，因此可能界面上的设置没有被更新
  ;; server-after-make-frame-hook 是在 frame 彻底准备好之后调用的函数

  (activities-mode +1)
  (activities-tabs-mode +1)

  :bind
  (("C-x w w" . activities-new)
   ("C-x w l" . activities-list)
   ("C-x w k" . activities-kill)
   ("C-x w d" . activities-discard)

   ("C-x w c" . activities-resume)
   ("C-x w r" . activities-rename)
   ("C-x w g" . activities-revert)
   ("C-x w s" . activities-suspend)

   ("C-x w e" . activities-define)
   ("C-x w a" . activities-save-all))

  ;; ("C-x w s" . activities-switch-buffer) ;; 有 consult 的情况下大概率不需要
  ;; ("C-x w w" . activities-switch)        ;; 使用 tab-bar 切换

  :config
  ;; 设置 activity-tab 的前缀
  (setq activities-name-prefix "")
  ;; 利用书签存储状态
  (setq activities-bookmark-store t)
  ;; 不能存为 bookmark 时警告
  (setq activities-bookmark-warnings t)
  ;; 在 suspend 时将该 activities 的 buffer 关闭

  (setq activities-kill-buffers nil)
  ;; 将 activity 名称设置为 frame 标题
  ;; Only applies when activities-tabs-mode is disabled.
  (setq activities-set-frame-name t)
  ;; 恢复 activity 时使用使用当前的 frame
  (setq activities-resume-into-frame 'current)


  ;; 设为 nil 则仅在退出时保存
  ;; t 时则在保存 buffer 时也保存
  (setq activities-always-persist nil)
  ;; 闲置 60s 后保存 activities
  (setq activities-mode-idle-frequency 60)

  ;; 令 tab-bar 不被 activities 影响 (太丑)
  (setq tab-bar-tab-face-function 'tab-bar-tab-face-default)

  ;; 该变量的颜色体现在 activities-resume 界面，而非 tab-bar
  (setq activities-annotation-colors '("blue" "red" 0.65)))


;; help-window-select 自动焦点 help 窗口

;; 考虑 popper+shackle 进行窗口弹出 + 弹出方式控制
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
  ((:map global-map
         ("C-z" . popper-toggle) ;; EAT 吞 C-z , 利用 meow 的 q 键退出，配置稳定后尝试修复
         ("M-z" . popper-toggle-type)
         ;; 使用 popper-kill-latest-popup 关闭已打开的弹窗缓冲区
         ("C-x w `" . popper-cycle)
         ("C-x w q" . popper-kill-latest-popup))
   (:repeat-map popper-repeat-map
                ("`" . popper-cycle)))

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

(provide 'core-window)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-window.el ends here
