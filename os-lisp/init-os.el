;;; init-os.el --- Init-OS -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; -----------------------------------------------------------------------------
;; 第一层：操作系统判断 (使用 defconst，因为 OS 启动后不会变)
;; -----------------------------------------------------------------------------

(defconst sys/nixosp
  ;; 检查 NixOS 特有的文件标志
  (file-exists-p "/etc/NIXOS")
  "判定当前是否为 NixOS 系统。")

(defconst sys/linuxp
  ;; 逻辑：是 Linux 内核，且不是 NixOS
  (and (eq system-type 'gnu/linux)
       (not sys/nixosp))
  "判定当前是否为常规 GNU/Linux 系统 (非 NixOS)。")

(defconst sys/macp
  (eq system-type 'darwin)
  "判定当前是否为 macOS 系统。")

(defconst sys/winp
  (eq system-type 'windows-nt)
  "判定当前是否为 Windows 系统。")

;; -----------------------------------------------------------------------------
;; 第二层：图形界面判断 (使用 defun，因为 Daemon 模式下状态可能变化)
;; -----------------------------------------------------------------------------

(defun sys/nixos-gui-p (&optional frame)
  "判定当前是否在 NixOS 的图形界面下。"
  (and (display-graphic-p frame)      
       (bound-and-true-p sys/nixosp)))

(defun sys/linux-gui-p (&optional frame)
  "判定当前是否在常规 Linux 的图形界面下。"
  (and (display-graphic-p frame)
       (bound-and-true-p sys/linuxp)))

(defun sys/mac-gui-p (&optional frame)
  "判定当前是否在 macOS 的图形界面下。"
  (and (display-graphic-p frame)
       (bound-and-true-p sys/macp)))

(defun sys/win-gui-p (&optional frame)
  "判定当前是否在 Windows 的图形界面下。"
  (and (display-graphic-p frame)
       (bound-and-true-p sys/winp)))

(when sys/nixosp
  (update-load-path '("os-lisp/nixos" "os-lisp/nixos/nixos-gui"))
  (require 'nixos-init))

(when sys/linuxp
  (update-load-path '("os-lisp/linux" "os-lisp/linux/linux-gui"))
  (require 'linux-init))

(when sys/macp
  (update-load-path '("os-lisp/mac" "os-lisp/mac/mac-gui"))
  (require 'mac-init))

(when sys/winp
  (update-load-path '("os-lisp/win" "os-lisp/win/win-gui"))
  (require 'win-init))

(provide 'init-os)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-os.el ends here
