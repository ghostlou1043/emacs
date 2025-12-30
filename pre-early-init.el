;;; pre-early-init.el --- Pre-Early-Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq debug-on-error t) ;; 不使用时连同 minimal-emacs-debug 一起注释

(setq minimal-emacs-ui-features '())
(setq minimal-emacs-debug t) ;; 
(setq minimal-emacs-optimize-startup-gc t)
(setq minimal-emacs-gc-cons-threshold (* 128 1024 1024))
(setq minimal-emacs-gc-cons-percentage gc-cons-percentage)
(setq minimal-emacs-gc-cons-threshold-restore-delay nil)
(setq minimal-emacs-optimize-file-name-handler-alist t)
(setq minimal-emacs-disable-mode-line-during-startup t)
;; 使用 straight+use-package
(setq minimal-emacs-package-initialize-and-refresh nil)
;; 待研究
(setq minimal-emacs-setup-native-compilation t)
(setq minimal-emacs-inhibit-redisplay-during-startup nil)
(setq minimal-emacs-inhibit-message-during-startup nil)
(setq minimal-emacs-user-directory user-emacs-directory)

;; 暂时不加载 elc 配置文件
(setq minimal-emacs-load-compiled-init-files nil)

;; 影响较大，待研究
;; (setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
;; (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))








;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; pre-early-init.el ends here
