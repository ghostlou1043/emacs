;;; core-dashboard.el --- Core Dashboard -*- lexical-binding: t -*-
;;; Commentary: Linux,MacOS,NixOS,Windows
;;; Code:

(use-package dashboard
  :ensure t
  :demand t
  :bind(:map dashboard-mode-map
             ("T" . telega)
             ("B" . butterfly)
             ("E" . elfeed)
             ("M" . emms))
  ;; dashboard-refresh-buffer
  :custom
  ;; 显示项图标
  (dashboard-set-heading-icons t)
  :config
  ;; 项目后端
  (if (1043/enable-projectile-p)
      (setq dashboard-projects-backend 'projectile)
    (setq dashboard-projects-backend 'project-el))

  ;; Org-agenda
  ;; https://github.com/emacs-dashboard/emacs-dashboard?tab=readme-ov-file#org-modes-agenda 后续进行配置
  ;; 显示未来一周的日程
  (setq dashboard-week-agenda t)
  ;; (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  ;; dashboard-match-agenda-entry
  ;; (setq dashboard-agenda-release-buffers t)
  ;; dashboard-agenda-sort-strategy org-agenda-sorting-strategy
  ;; dashboard-agenda-prefix-format
  ;; dashboard-agenda-time-string-format
  ;; dashboard-agenda-tags-format

  ;; display icons on both GUI and terminal
  (setq dashboard-display-icons-p t)
  ;; use `nerd-icons' package 使用图标包
  (setq dashboard-icon-type 'nerd-icons)
  ;; 显示文件图标
  (setq dashboard-set-file-icons t)

  ;; (setq dashboard-startup-banner "~/.config/emacs/VisLain.gif")
  ;; (setq dashboard-startup-banner '("~/.config/emacs/VisLain.gif" . ""))
  (setq dashboard-startup-banner 1)
  (setq dashboard-banner-logo-title "    Hello, 1043.\nWelcome to the Wired.")

  ;; 显示跳转快捷键
  (setq dashboard-show-shortcuts t)
  ;; 横向居中内容
  (setq dashboard-center-content t)
  ;; 垂直居中内容
  (setq dashboard-vertically-center-content t)
  ;; 不开启循环
  (setq dashboard-navigation-cycle nil)

  ;; 未指定显示项目显示条数时的默认条数
  (setq dashboard-items-default-length 5)
  ;; 显示项目
  (setq dashboard-items
        '((recents   . 5)
          (bookmarks . 5)
          (agenda    . 5)
          (projects  . 5)))
  ;; 项目快捷键显示设置及键位设置
  (setq dashboard-heading-shorcut-format " [%s]")
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")
                                   (registers . "e")))

  ;; 查看 dashboard-startupify-list 以获取所有可用的部件
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-banner-title
          dashboard-insert-init-info
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-items
          dashboard-insert-newline
          dashboard-insert-footer))

  (setq dashboard-navigator-buttons
        '((("" "Telega" "Telegrame"
            (lambda (&rest _) (telega))
            warning "[" "]")
           ("" "Elfeed" "Browse RSS Feeds"
            (lambda (&rest _) (elfeed))
            warning "[" "]")
           ("" "EMMS" "Emacs Multi-Media System"
            (lambda (&rest _) (emms))
            warning "[" "]")
           ("" "Butterfly" "Real world programming!"
            (lambda (&rest _) (butterfly))
            warning "[" "]"))))

  (setq dashboard-item-names '(("Recent Files:"               . "Recently opened files:")
                               ("Agenda for today:"           . "Today's agenda:")
                               ("Agenda for the coming week:" . "Agenda:")))

  ;; Emacs Daemon 下新建 frame 或 tab 都使用 dashboard
  (setq initial-buffer-choice 'dashboard-open)
  (setq tab-bar-new-tab-choice 'dashboard-open)
  ;; 刷新一次确保图标正常显示
  (add-hook 'server-after-make-frame-hook 'dashboard-open)

  ;; dashboard 界面隐藏 mode-line
  (add-hook 'dashboard-mode-hook (lambda () (setq-local mode-line-format nil)))

  ;; 与 elpaca 适配
  (when (boundp 'elpaca-after-init-hook)
    (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
    (add-hook 'elpaca-after-init-hook #'dashboard-initialize))

  (dashboard-setup-startup-hook))

(provide 'core-dashboard)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-dashboard.el ends here
