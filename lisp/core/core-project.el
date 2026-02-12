;;; core-project.el --- Core Project -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :if (1043/enable-projectile-p)
  :bind-keymap ("C-x p" . projectile-command-map)
  :init
  (which-key-add-key-based-replacements "C-x p" "Projectile")
  (setq projectile-mode-line-prefix "") ;; projectile-mode-line-function 自定义输出，prefix 选项对其无效
  (setq projectile-project-search-path '("~/nixos-config/" "~/.config/emacs/"))
  ;; (setq projectile-project-search-path '("~/projects/" "~/work/" ("~/github" . 1)))
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'projectile-mode)
    (add-hook 'after-init-hook #'projectile-mode))
  :config
  (setq projectile-auto-discover nil)
  (setq projectile-cleanup-known-projects nil) ;; 为了 tramp 不自动清理
  (setq projectile-git-use-fd t)

  ;; projectile-project-root-functions

  ;; 在版本控制配置中添加忽略项（例如 .gitignore ），无须 Projectile 可能提供的额外忽略/取消忽略/排序功能
  (setq projectile-indexing-method 'alien) ;; 不对外部命令返回的文件进行任何处理或排序，性能最好
  ;; 如果没有使用 alien 下述选项排序生效
  (setq projectile-sort-order 'recently-active) ;; 按最近活动的缓冲区和最近打开的文件排序
  ;; 启用缓存
  (setq projectile-enable-caching t)
  ;; 使缓存持久生效
  (setq projectile-enable-caching 'persistent)
  ;; 自动更新缓存
  (setq projectile-auto-update-cache t)
  ;; 远程缓存 10 分钟再过期，避免多次请求造成卡顿
  (setq projectile-file-exists-remote-cache-expire 600)
  ;; 不在项目目录中使用 Projectile 功能需要确认
  ;; 如果你在项目外部调用 Projectile，当前目录将被 Projectile 视为项目根目录
  ;; 不要在家目录使用 Projectile
  (setq projectile-require-project-root 'prompt)

  ;; 对于经常需要在切换项目时调用不同操作的人来说，这是推荐选项
  (setq projectile-switch-project-action 'projectile-commander)

  
  (setq projectile-dynamic-mode-line t)
  (setq projectile-mode-line-function '(lambda () (format " P[%s]" (projectile-project-name))))

  (add-hook 'project-find-functions #'project-projectile))



(provide 'core-project)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-project.el ends here
