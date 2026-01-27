;;; core-window.el --- Core Window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

;; (use-package popper
;;   :ensure t
;;   :after (shackle)
;;   ;; :bind
;;   ;; (:map global-map
;;   ;;       ("C-;" . popper-toggle)
;;   ;;       ("C-'" . popper-cycle)
;;   ;;       ("C-:" . popper-toggle-type))
;;   :config
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"                 ; 匹配 *Messages* buffer
;;           ;; ;; ("Output\\*$" . hide)           ; 匹配以 Output* 结尾的 buffer, 并且抑制了弹窗
;;           "\\*gt-result\\*"
;;           "\\*Help\\*"
;;           help-mode
;;           "^\\*helpful.*\\*$"
;;           helpful-mode
;;           "\\*eldoc\\*"
;;           compilation-mode                 ; 所有 compilation-mode 的 buffers
;;           "^\\*eshell.*\\*$"
;;           eshell-mode
;;           "^\\*shell.*\\*$"
;;           shell-mode
;;           "^\\*term.*\\*$"
;;           term-mode
;;           "^\\*eat.*\\*$"
;;           eat-mode
;;           "^\\*vterm.*\\*$"
;;           vterm-mode
;;           ))  ; 匹配 vterm buffers
;;   ;; (setq popper-group-function #'popper-group-by-perspective)
;;   (setq popper-group-function #'popper-group-by-projectile)
;;   (setq popper-echo-dispatch-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
;;   (setq popper-echo-lines 2)  ; 设置为 2 行显示
;;   (setq popper-echo-transform-function
;;         (lambda (name)
;;           (if (> (length name) 10)
;;               (concat (substring name 0 10) "…") ; 截断长名称
;;             name)))  ; 保留短名称
;;   ;; (setq popper-mode-line nil) ;; 禁止 mode-line 显示 pop
;;   (setq popper-display-control nil) ;; 使用 Shackle 控制窗口的弹出
;;   (popper-mode +1)
;;   (popper-echo-mode +1))

(provide 'core-window)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-window.el ends here
