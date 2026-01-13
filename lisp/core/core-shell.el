;;; core-shell.el --- Core Shell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eat
  :ensure t
  :hook
  ((eshell-load . eat-eshell-mode)
   (eat-exit . meow-normal-mode))
  :bind
  (:map global-map
        ("C-x t" . eat)))

(provide 'core-shell)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-shell.el ends here
