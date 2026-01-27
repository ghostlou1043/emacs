;;; init-org-modern.el --- Init Org Modern -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :config
  ;; ;; 设置org mode标题以及美级标题行的大小
  ;; (org-document-title ((t (:height 1.75 :weight bold))))
  ;; (org-level-1 ((t (:height 1.4 :weight bold))))
  
  ;; Edit settings
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis "…")
  (setq org-startup-indented t) ;; org-modern-indent 需要
  (setq org-src-preserve-indentation nil) ;; Org-mode 自动调整块缩进使 org-modern-indent 显示正常
  ;; 通过 org-entities 变量配置 org-pretty-entities 的效果
  (setq org-pretty-entities t)            ;; 上下标和希腊字母美化等等
  (setq org-use-sub-superscripts '{})     ;; 上下标需要 {}
  (setq org-hidden-keywords nil)          ;; 不隐藏关键字 #+TITLE 等
  
  ;; 多使用 org-insert-structure-template 来智能插入代码块
  ;; org-structure-template-alist 没有配置或者 org-tempo
  ;; prettify-symbols-mode

  ;; Org-agenda
  (setq org-agenda-tags-column 0)
  )


(use-package org-modern
  :ensure t
  :config
  ;; (defface org-modern-symbol nil)    ;; 必要的情况下用于修改字体
  ;; (setq org-pretty-entities t)       ;; 美化特殊字符实体 如：\alpha 直接显示为 α

  (setq org-modern-table nil)  ;; 美化表格 不如默认好看
  (setq org-modern-table-vertical 1)
  (setq org-modern-table-horizontal 0.1)

  (setq org-modern-timestamp t) ;; 美化时间戳 待定
  (setq org-hide-emphasis-markers t) ;; 隐藏强调标记
  (setq org-modern-horizontal-rule t);; 美化水平标尺 ----- 变成一条分割线
  

  (setq org-modern-tag nil)
  (setq org-modern-todo nil)
  (setq org-modern-habit nil)
  (setq org-modern-progress nil)     ;; 美化进度
  (setq org-modern-footnote nil)     ;; 影响编辑
  (setq org-modern-block-name nil)
  (setq org-modern-block-fringe nil) ;; 代码块边缘美化 使用 org-modern-indent 平替
  (setq org-modern-radio-target nil)
  (setq org-modern-internal-target nil)

  ;; (setq org-modern-keyword        ;; 待定 需要有好看的图标
  ;;       (quote ((\"options\" . \"🔧\")
  ;;               (t . t))))
  (setq org-modern-keyword nil)       ;; 待定 需要有好看的图标
  (setq org-modern-checkbox nil)     ;; 待定 需要有好看的图标
  (setq org-modern-priority nil)     ;; 待定 需要有好看的图标
  ;; (?A . "❗")
  ;; (?B . "⬆")
  ;; (?C . "⬇")
  ;; (?D . "☕")
  ;; (?1 . "⚡")
  ;; (?2 . "⮬")
  ;; (?3 . "⮮")
  ;; (?4 . "☕")
  ;; (?I . "Important")
  (setq org-modern-list              ;; 待定 需要有好看的图标
        '((?- . "-")  ;; 这一行被注释了，表示减号列表不美化
          (?* . "•")     ;; 星号列表变成圆点
          (?+ . "‣")))   ;; 加号列表变成三角形

  ;; org-modern-replace-stars "◉○◈◇✳" ;; 待定 需要有好看的图标

  ;; org-modern-fold-stars            ;; 暂时使用
  ;; org-modern-star 'fold            ;; 暂时使用
  ;; org-modern-hide-stars 'leading
  (setq org-modern-hide-stars nil)    ;; 因开启 org-indent-mode 此功能失效 直接设为nil
  (global-org-modern-mode +1))

(use-package org-modern-indent
  :ensure  (org-modern-indent
            :host github
            :repo "jdtsmith/org-modern-indent")
  :if org-startup-indented
  :config
  ;; 90 是优先级（数字越大，越晚执行）
  ;; 确保 org-modern-indent 是最后一个启动的。
  ;; 必须等 org-indent 和 org-modern 都准备好了，它才能最后进场进行“对齐修正”。
  ;; 如果 org-mode-indent 启动早了，修正就会失效。
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config

  (setq org-appear-autoemphasis t) ;; 如果非 nil 且 org-hide-emphasis-markers 开启，切换强调标记
  (setq org-appear-autolinks t) ;; 如果非 nil 且 org-link-descriptive 开启，切换链接
  (setq org-appear-autosubmarkers t) ;; 如果非 nil 且 org-pretty-entities 开启，切换下标和上标
  (setq org-appear-autoentities t) ;; 如果非 nil 且 org-pretty-entities 开启，切换特殊符号
  (setq org-appear-inside-latex t) ;; 如果非 nil，则在 LaTeX 片段中切换实体和下标/上标
  (setq org-appear-autokeywords nil)   ;; 如果非 nil 且 org-hidden-keywords 开启，则在 org-hidden-keywords 中切换关键词

  (setq org-appear-trigger 'always) ;; when to toggle elements
  ;; 切换前的延迟秒数
  (setq org-appear-delay 0))



(provide 'init-org-modern)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-org-modern.el ends here
