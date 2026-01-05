;;; core-visual.el --- Core-Visual -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package visual-fill-column
  :ensure t
  :hook ((visual-line-mode-hook . visual-fill-column-for-vline))
  ;; visual-fill-column-toggle-center-text 与 visual-line-mode 需要设置快捷键
  :config
  (setq visual-fill-column-center-text nil)
  (setq visual-fill-column-fringes-outside-margins t)
  (setq visual-fill-column-enable-sensible-window-split t)
  ;; (setq visual-fill-column-extra-text-width '(10.10)) 好像用不到
  
  (setq visual-fill-column-adjust-for-text-scale t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
      
  (setq visual-fill-column-width nil))



(provide 'core-visual)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-visual.el ends here





