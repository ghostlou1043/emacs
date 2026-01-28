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
  (setq desktop-restore-frames t) ;; 似乎对 daemon 没有破坏性影响，且非 daemon 下可以用于恢复布局

  ;; (setq desktop-dirname) ;; 保持默认
  ;; (setq desktop-base-file-name) ;; 保持默认
  
  (setq desktop-auto-save-timeout 60) 
  (setq desktop-save t))

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (("C-x w u" . winner-undo)
   ("C-x w r " . winner-redo)))

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
        ("C-x w k" . popper-kill-latest-popup))
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

(use-package eyebrowse
  :ensure t
  :hook (elpaca-after-init . eyebrowse-mode)
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-x w"))
  :bind (:map eyebrowse-mode-map
              ("C-x w b" . eyebrowse-switch-to-window-config)
              ("C-x w b" . eyebrowse-prev-window-config)
              ("C-x w f" . eyebrowse-next-window-config)
              ("C-x w l" . eyebrowse-last-window-config)

              ("C-x w n" . eyebrowse-rename-window-config)
              ("C-x w c" . eyebrowse-close-window-config)
              ("C-x w w" . eyebrowse-create-window-config)
              )
  :config
  ;; 使 eyebrowse 的布局切换对 treemacs 等也生效
  (add-to-list 'window-persistent-parameters '(window-side . writable))
  (add-to-list 'window-persistent-parameters '(window-slot . writable))

  ;; (frame-parameter nil 'name)
  ;; (set-frame-parameter nil 'name "Main")

  ;; 不使用 eyebrowse-setup-opinionated-keys , 避免 M-0~9 被占用
  (eyebrowse-mode +1)
  )

(provide 'core-window)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-window.el ends here
