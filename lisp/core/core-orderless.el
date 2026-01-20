;;; core-orderless.el --- Init Vertico -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 提供 orderless 补全匹配风格
(use-package orderless
  :ensure t
  :config
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)

  ;; 优先级 overrides > defaults > completion-styles
  ;; completion-at-point-functions 会随 major-mode 改变（即存在全局与局部），
  ;; 部分功能如 eglot,lsp-mode 也会追加 completion-at-point-functions
  (setq completion-category-overrides '((file (styles partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (setq completion-category-defaults nil) ;; Disable defaults, use our settings
  (setq completion-styles '(orderless basic))

  (setq completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(provide 'core-orderless)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-orderless.el ends here
