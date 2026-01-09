;;; core-magit.el --- Core Magit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  ;; :custom
  ;; (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)  ; 在单独的全屏窗口中显示 magit
  ;; (magit-commit-show-diff nil)  ; 提交时不自动显示 diff，提高提交效率
  ;; (magit-save-repository-buffers 'dontask)  ; 自动保存所有改动的 buffer，避免频繁询问
  ;; (magit-diff-refine-hunk t)  ; 在 diff 中高亮显示精确修改的部分（字级别）
  ;; (magit-section-initial-visibility-alist '((stashes . hide) (untracked . show)))  ; 默认隐藏 stash 区域，显示未追踪的文件
  ;; (magit-ediff-dwim-show-on-hunks t)
  :bind
  (:map global-map
        ("C-x g" . magit-status))  ; 绑定 C-x g 快捷键到 magit-status

  :config
  (setq magit-refresh-status-buffer t)  ;; 非必要不设置为 nil 以提高性能
  (setq magit-view-git-manual-method 'woman))

;; (setq magit-submodule-list-columns
;;       '(("Name" 25 magit-submodule-name)
;;         ("Path" 40 magit-submodule-path)
;;         ("Url" 40 magit-submodule-url)))
;; 当打开 magit 界面时，更新仓库状态
;; (add-hook 'magit-status-sections-hook 'magit-insert-repo-status))
;; 配置magit进一步的行为
;; (setq magit-push-always-verify nil "在推送时不总是要求验证")


;; (use-package magit-delta
;;   :ensure t
;;   :hook (magit-mode . magit-delta-mode)  ; 在 Magit 模式中启用 magit-delta
;;   :custom
;;   (magit-delta-hide-plus-minus-markers nil))  ; 配置项，保持加号和减号标记可见

;; (use-package diff-hl
;;   :ensure t
;;   :custom
;;   (diff-hl-draw-borders nil)
;;   :hook
;;   ((after-init . global-diff-hl-mode)
;;    (after-init . global-diff-hl-show-hunk-mouse-mode)
;;    ;; (dired-mode         . diff-hl-dired-mode-unless-remote)
;;    (magit-pre-refresh . diff-hl-magit-pre-refresh)   ; Magit刷新前执行
;;    (magit-post-refresh . diff-hl-magit-post-refresh))  ; Magit刷新后执行
;;   :config
;;   ;; (diff-hl-flydiff-mode 1)     ; 实时高亮变更，耗费性能，不用
;;   (setq diff-hl-disable-on-remote t) ;; 远程可能影响性能，我觉得
;;   (diff-hl-margin-mode +1))

;; (use-package git-gutter
;;   :ensure t
;;   :demand t
;;   :bind
;;   (("C-t v v" . git-gutter:set-start-revision)
;;    ("C-t v p" . git-gutter:popup-hunk)
;;    ("C-t v s" . git-gutter:stage-hunk)
;;    ("C-t v r" . git-gutter:revert-hunk)
;;    ("C-t v m" . git-gutter:mark-hunk)
;;    ("C-c C-n" . git-gutter:next-hunk) ;; C-x C-n 可用于备选
;;    ("C-c C-p" . git-gutter:previous-hunk))

;;   :config
;;   (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
;;   (add-to-list 'git-gutter:update-commands 'other-window)
;;   ;; (setq git-gutter:visual-line t) ;; 对于折行可能需要，暂时不用
;;   ;; (setq git-gutter:handled-backends '(git hg bzr svn))
;;   (setq git-gutter:handled-backends '(git))
;;   (setq git-gutter:ask-p nil) ;; 禁用提交和还原的确认提示
;;   (setq git-gutter:verbosity 0) ;; 设置 git-gutter 的日志和消息级别
;;   (setq git-gutter:hide-gutter nil) ;; 不去隐藏边栏，避免造成抖动
;;   ;; If you enable global minor mode
;;   (global-git-gutter-mode +1))



(provide 'core-magit)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-magit.el ends here
