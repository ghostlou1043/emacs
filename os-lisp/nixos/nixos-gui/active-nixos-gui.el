;;; active-nixos-gui.el --- Active-NixOS-GUI -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; 激活 Emacs 的 GUI 相关插件包
(defun sys/active-nixos-gui (&optional frame)
  (let ((current-frame (or frame (selected-frame))))
    (when (sys/nixos-gui-p current-frame)
      (fontaine-mode +1))))

(add-hook 'server-after-make-frame-hook #'sys/active-nixos-gui)

(unless (or (daemonp) (not (display-graphic-p)))
  (if (boundp 'elpaca-after-init-hook)
    (add-hook 'elpaca-after-init-hook #'sys/active-nixos-gui)
    (add-hook 'after-init-hook #'sys/active-nixos-gui)))
  

;; elpaca-after-init-hook
;; (if (boundp 'elpaca-after-init-hook)
;;     ;; 场景 A: Elpaca 存在，挂载到 Elpaca 完成后的钩子上
;;     (add-hook 'elpaca-after-init-hook #'my/load-custom-file)
;;   ;; 场景 B: Elpaca 不存在，挂载到标准的 Emacs 启动钩子上
;;   (add-hook 'after-init-hook #'my/load-custom-file))

(provide 'active-nixos-gui)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; active-nixos-gui.el ends here

;; (when sys/nixosp
;;   ;; 加载 Emacs 的内置 GUI 设置选项
;;   (defun sys/setup-nixos-gui-frame (&optional frame)   ;; 判断是否图形界面，统一加载 Emacs 内置的 GUI 设置，通过钩子 server-after-make-frame-hook 触发
;;     (let ((current-frame (or frame (selected-frame))))
;;       (when (sys/nixos-gui-p current-frame)
;;         (push '(menu-bar-lines . 0) default-frame-alist)
;;         (push '(tool-bar-lines . 0) default-frame-alist))))
;;   (add-hook 'server-after-make-frame-hook #'sys/setup-nixos-gui-frame)
;; 

;; 
;; (when sys/nixosp
;;   (unless (daemonp) ; 检查当前 Emacs 实例是否是 daemon 如果不是则执行函数
;;     (sys/setup-nixos-gui-frame))
;;   (unless (daemonp) ; 检查当前 Emacs 实例是否是 daemon 如果不是则执行函数
;;     (sys/load-nixos-gui-plugin)))



