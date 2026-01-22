;;; nixos-cn.el --- NixOS-EMT -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emt
  :ensure (emt
           :host github
           :repo "roife/emt"
           :files ("*.el" "module/*" "module"))
  :config
  (if (boundp 'elpaca-after-init-hook)
      (add-hook 'elpaca-after-init-hook #'emt-mode)
    (add-hook 'after-init-hook #'emt-mode))
  
  (setq emt-use-cache t)
  (setq emt-cache-lru-size 50)
  ;; https://github.com/Master-Hash/ewt-rs
  (setq emt-lib-path (expand-file-name "modules/libewt.so" user-emacs-directory)))


(provide 'nixos-cn)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; nixos-cn.el ends here
