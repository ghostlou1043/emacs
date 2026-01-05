;;; nixos-pass.el --- NixOS-Pass -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package password-store
  :ensure t)

(use-package pass
  :ensure t
  :config
  (setq pass-suppress-confirmations nil)
  (setq pass-username-fallback-on-filename t)
  (setq password-store-password-length 16))

(provide 'nixos-pass)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; nixos-pass.el ends here
