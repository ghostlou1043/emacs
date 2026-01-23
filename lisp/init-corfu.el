;;; init-corfu.el --- Init Corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 提供现代化的 buffer 中补全前端
(use-package corfu
  :ensure t
  :unless (1043/enable-lsp-bridge-p)
  :init
  (global-corfu-mode +1)

  :bind (:map corfu-map
              ;; ("M-TAB")	corfu-expand
              ;; ("M-g")	corfu-info-location
              ;; ("M-h")	corfu-info-documentation
              ;; ("M-SPC")	corfu-insert-separator

              ("C-a" . corfu-prompt-beginning)
              ("C-e" . corfu-prompt-end)
              ("M-a" . corfu-first)
              ("M-e" . corfu-last)
              ("M-v" . corfu-scroll-down)
              ("C-v" . corfu-scroll-up))
  :config
  (keymap-unset corfu-map "RET")

  (setq corfu-cycle nil)              ;; Enable cycling for `corfu-next/previous'
  (setq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (setq corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (setq corfu-preview-current 'insert)


  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; (setq corfu-auto nil
  ;;       corfu-auto-delay 0.2 ;; When corfu-auto t
  ;;       corfu-auto-trigger "." ;; Custom trigger characters
  ;;       corfu-quit-no-match 'separator) ;; or t

  ;; 我们在 Minibuffer（底部命令栏）里希望越智能越好（用 Orderless，支持模糊、乱序）。
  ;; 但在代码编辑区（Corfu 弹窗），如果开启了“自动补全”，Orderless 可能太重了，导致卡顿。而且有时候我们只想要简单的“前缀匹配”（打什么出什么）。
  ;; (add-hook 'corfu-mode-hook
  ;;         (lambda ()
  ;;           ;; setq-local 意味着这些设置只在当前开启了 Corfu 的缓冲区生效
  ;;           ;; 不会影响 Minibuffer
  ;;           (setq-local completion-styles '(basic partial-completion emacs22)
  ;;                       completion-category-overrides '((file (styles partial-completion)))
  ;;                       completion-category-defaults nil
  ;;                       completion-pcm-leading-wildcard t)))

  ;; (add-hook 'corfu-mode-hook
  ;;         (lambda ()
  ;;           ;; setq-local 意味着这些设置只在当前开启了 Corfu 的缓冲区生效
  ;;           ;; 不会影响 Minibuffer
  ;;           (setq-local completion-styles '(basic)
  ;;                       completion-category-overrides nil
  ;;                       ;; completion-category-defaults nil
  ;;                       )))

  (setq corfu-echo-delay '(0.3 . 0.3))
  (setq corfu-popupinfo-delay '(0.3 . 0.3))
  ;; (corfu-echo-mode +1)
  (corfu-history-mode +1)
  (corfu-popupinfo-mode +1))

;; 提供 corfu 候选项图标
(use-package nerd-icons-corfu
  :ensure t
  :unless (1043/enable-lsp-bridge-p)
  :after corfu
  ;; :init
  ;; (cl-loop for range in '((#xe000 . #xf8ff) (#xf0000 . #xfffff))
  ;;    return (set-fontset-font t range "IosevkaTerm Nerd Font Mono"))
  :config

  ;;   (defun nerd-icons-corfu-formatter (_)
  ;;   "A margin formatter for Corfu, adding icons.
  ;; 
  ;; It receives METADATA, ignores it, and outputs a function that takes a candidate
  ;; and returns the icon."
  ;;   (and-let* ((kindfunc (plist-get completion-extra-properties :company-kind)))
  ;;     (lambda (cand)
  ;;       (let* ((result (funcall kindfunc cand))
  ;;              (kind (if (stringp result) (intern (downcase result)) result))
  ;;              (glyph (nerd-icons-corfu--get-by-kind kind cand)))
  ;;         (concat
  ;;          (and (display-graphic-p) nerd-icons-corfu--space)
  ;;          glyph
  ;;          nerd-icons-corfu--space)))))
  
  ;; (setq nerd-icons-corfu-mapping
  ;;      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
  ;;        (Function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; 提供额外的补全源
(use-package cape
  :ensure t
  :unless (1043/enable-lsp-bridge-p)
  ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init

  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev) ;; 交给 其他按键
  (add-hook 'completion-at-point-functions #'cape-file)

  ;; 看情况配置
  ;; (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


;; hippie-expand 涵盖了 dabbrev 的功能，但 corfu 也可以替代 hippie-expand 从而直接使用 dabbrev


;;
;; (use-package corfu
;;   :straight t
;;   :after fussy
;;   :unless kaladin/lsp-bridge
;;   :init
;;   (global-corfu-mode +1)
;;   (corfu-history-mode +1)
;;   (corfu-echo-mode +1)
;;   (corfu-popupinfo-mode +1)
;;   :hook
;;   ((eshell-mode . (lambda () (setq-local corfu-auto nil)(corfu-mode)))
;;    (vterm-mode . (lambda () (setq-local corfu-auto nil)(corfu-mode))))
;;   :bind
;;   (:map corfu-map
;;         ("M-SPC" . corfu-insert-separator)
;;         ("C-q" . corfu-quick-insert)
;;         ("RET" . nil))
;;   :config
;;   (setq corfu-cycle t
;; 	    corfu-auto nil
;; 	    corfu-auto-delay  0
;; 	    corfu-separator ?\s
;; 	    corfu-preview-current nil
;; 	    corfu-preselect 'valid
;; 	    corfu-on-exact-match 'insert
;; 	    corfu-scroll-margin 2
;; 	    corfu-auto-prefix 2
;; 	    corfu-quit-at-boundary 'separator
;; 	    corfu-quit-no-match 'separator)
;;   ;; corfu-echo
;;   (setq corfu-echo-delay '(0 . 0))
;;   ;; corfu-popupinfo
;;   (setq corfu-popupinfo-hide nil)
;;   (setq corfu-popupinfo-delay '(0 . 0)))
;;
;;
;; (use-package hippie-exp
;;   :straight (:type built-in)
;;   :bind ([remap dabbrev-expand] . hippie-expand)
;;   :config
;;   (setq hippie-expand-try-functions-list
;;         '(try-complete-file-name-partially
;;           try-complete-file-name
;;           try-expand-dabbrev
;;           try-expand-dabbrev-all-buffers
;;           try-expand-dabbrev-from-kill)))
;;

(provide 'init-corfu)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-corfu.el ends here
