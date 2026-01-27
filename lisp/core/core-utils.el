;;; core-utils.el --- Core-Utils -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package restart-emacs
  :ensure t
  ;; (defvar 1043/restart-emacs-map (make-sparse-keymap)
  ;;   "Keymap for `restart-emacs'.")
  ;; (which-key-add-key-based-replacements "C-c C-q" "restart-emacs")
  ;; :bind-keymap ("C-c C-q" . 1043/restart-emacs-map)
  ;; :bind (:map kaladin/restart-emacs-map
  ;;             ("c" . restart-emacs)
  ;;             ("d" . kaladin/restart-emacs-with-debug)
  ;;             ("q" . kaladin/restart-emacs-with-Q)
  ;;             ("a" . kaladin/restart-emacs-with-args)))
  :config
  (defun lou/restart-emacs-with-debug ()
    "Restart Emacs with --debug-init."
    (interactive)
    (restart-emacs '("--debug-init")))
  (defun lou/restart-emacs-with-Q ()
    "Restart Emacs with -Q."
    (interactive)
    (restart-emacs '("-Q")))
  (defun lou/restart-emacs-with-args ()
    "Restart Emacs with arguments provided by user."
    (interactive)
    (let ((args (read-string "Enter startup args: ")))
      (restart-emacs (split-string-and-unquote args)))))

(use-package async
  :ensure t
  :config
  ;; (async-bytecomp-package-mode +1)  ;;
  (dired-async-mode +1))

(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))

(use-package inheritenv
  :ensure t)

(use-package daemons
  :ensure t
  :config
  ;; default nil
  (setq daemons-always-sudo t))

(use-package tldr
  :ensure t)

;; need mplayer
(use-package nyan-mode
  :ensure (nyan-mode
           :host github
           :repo "zakudriver/nyan-mode"
           :files ("nyan-mode.el" "img" "mus")
           )
  :config
  (setq nyan-cat-face-number 1)
  (setq nyan-animate-nyancat t)
  (setq nyan-animation-frame-interval 0.2)
  (setq nyan-bar-length 32)
  (setq nyan-wavy-trail t)
  (setq nyan-cat-face-number 1)
  (setq nyan-minimum-window-width 64))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))

(use-package ement
  :ensure t)

(use-package envrc
  :ensure t
  :init
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'envrc-global-mode)
    (add-hook 'after-init-hook #'envrc-global-mode))
  :config
  ;; 静默启动，不显示详细加载信息
  (setq envrc-show-summary-in-minibuffer t))

(use-package plz
  :ensure t)

(use-package pdd
  :ensure t)

(use-package snow
  :ensure t)

(use-package xkcd
  :ensure t)

(use-package focus
  :ensure t)

(use-package request
  :ensure t)

(use-package emacsql
  :ensure t)

(use-package keycast
  :ensure t)

(use-package jsonrpc
  :ensure t)

(use-package posframe
  :ensure t)

(use-package qrencode
  :ensure t)

(use-package transient
  :ensure t)

(use-package with-editor
  :ensure t)


(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "IosevkaTerm Nerd Font Mono"))

(use-package rainbow-mode
  :ensure t
  :config
  ;; 有机会改写成按照其他模式触发
  ;; (rainbow-mode +1)
  )

(use-package wakatime-mode
  :ensure t
  :delight
  :config
  ;; 学习如何配置 .wakatime .wakatime-project
  (global-wakatime-mode +1))

(use-package rainbow-identifiers
  :ensure t)

;; ------- 造成终端emacs画面错乱
;; (use-package ewth
;;   :ensure (ewth
;;            :host github
;;            :repo "ISouthRain/ewth.el")
;;   :if 1043/location
;;   :init
;;   (if (boundp 'elpaca-after-init-hook)
;;       (add-hook 'elpaca-after-init-hook #'ewth-mode)
;;     (add-hook 'after-init-hook #'ewth-mode))
;;   :config
;;   (setq ewth-url (format "http://wttr.in/%s?format=1" 1043/location))
;;   (setq ewth-update-interval 1800))
;; -------

(use-package atomic-chrome
  :ensure t
  :config
  ;; (setq atomic-chrome-url-major-mode-alist
  ;;     '(("github\\.com" . gfm-mode)
  ;;       ("redmine" . textile-mode)))
  (setq atomic-chrome-buffer-open-style 'frame)
  (setq atomic-chrome-default-major-mode 'text-mode)
  (setq atomic-chrome-enable-auto-update t)
  (atomic-chrome-start-server))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode +1))

(use-package helpful
  :ensure t
  :bind
  (:map global-map
        ("C-h f" . helpful-callable)    ;; 绑定 C-h f 到 helpful-callable，用于查看函数或宏的帮助
        ("C-h v" . helpful-variable)    ;; 绑定 C-h v 到 helpful-variable，用于查看变量的帮助
        ("C-h k" . helpful-key)         ;; 绑定 C-h k 到 helpful-key，用于查看按键绑定的帮助
        ("C-h x" . helpful-command)     ;; 绑定 C-h x 到 helpful-command，用于查看命令的帮助
        ("C-h ." . helpful-at-point)))  ;; 绑定 C-h . 到 helpful-at-point，用于查看当前点 (point) 的项目帮助

;; Track Emacs commands frequency
;; use `keyfreq-show' to see how many times you used a command.
(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands ;; FIX ME.
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line))
  (keyfreq-mode +1)
  (keyfreq-autosave-mode +1))

(use-package link-hint
  :ensure t
  ;; arfd 留给 eglot
  :bind (:map global-map
              ("C-c l o" . link-hint-open-link) ; 快速打开链接
              ("C-c l c" . link-hint-copy-link) ; 快速复制链接
              ("C-c l ." . link-hint-open-link-at-point) ;; 打开光标处的链接。
              ("C-c l ," . link-hint-copy-link-at-point) ;; 将光标处的链接复制到剪贴板。
              ("C-c l m" . link-hint-open-multiple-links) ;; 选择多个链接并打开，直到按下一个非 avy 键。
              ("C-c l l" . link-hint-open-all-links)) ;; 打开当前缓冲区中所有可见链接。
  :config
  (setq link-hint-restore t))

;; 
(use-package which-key
  :ensure t
  :delight
  :bind
  (:map global-map
        ("C-x <" . which-key-show-previous-page-cycle)
        ("C-x >" . which-key-show-next-page-cycle)
        ("C-h <" . which-key-show-previous-page-cycle)
        ("C-h >" . which-key-show-next-page-cycle)
        ("C-c <" . which-key-show-previous-page-cycle)
        ("C-c >" . which-key-show-next-page-cycle)
        ("C-x C-<" . which-key-show-previous-page-cycle)
        ("C-x C->" . which-key-show-next-page-cycle)
        ("C-h C-<" . which-key-show-previous-page-cycle)
        ("C-h C->" . which-key-show-next-page-cycle)
        ("C-c C-<" . which-key-show-previous-page-cycle)
        ("C-c C->" . which-key-show-next-page-cycle))

  :config
  (setq which-key-separator " → ")
  (setq which-key-max-description-length 32)
  (setq which-key-use-C-h-commands nil)
  (setq which-key-popup-type 'minibuffer)
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.5)
  (setq which-key-max-display-columns 5)
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (setq which-key-show-prefix 'top)
  (which-key-mode +1))

(provide 'core-utils)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-utils.el ends here
