;;; nixos-utils.el --- NixOS-Utils -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :ensure t
  :config
  (setq xclip-select-enable-clipboard t)
  (when (sys/x11-p)
    (setq xclip-method 'xclip)
    (setq xclip-program "xclip"))

  (when (sys/wayland-p)
    (setq xclip-method 'wl-copy)
    (setq xclip-program "wl-copy"))

  (xclip-mode +1))

;; (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys)) ;; 升到emacs31时研究

;; Clipetty 需要Tmux+SSH时使用


(provide 'nixos-utils)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; nixos-utils.el ends here
