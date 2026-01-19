;;; init-corfu.el --- Init Corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 提供现代化的 buffer 中补全前端
(use-package corfu
  :ensure t
  :if (1043/enable-corfu-p)
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
  ;; (setq corfu-preview-current 'insert);; 避免双重补全

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
  )

;; 提供 corfu 候选项图标
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

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

;; 提供额外的补全源
(use-package cape
  :ensure t
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


;; (use-package cape
;;   :straight t
;;   :unless kaladin/lsp-bridge
;;   :after corfu
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   (defun kaladin/eglot-capf ()
;;     (setq-local completion-at-point-functions
;;                 (list (cape-capf-super
;;                        #'eglot-completion-at-point
;;                        ;; #'tempel-expand
;;                        #'cape-file))))
;;   (add-hook 'eglot-managed-mode-hook #'kaladin/eglot-capf)
;;   :bind-keymap ("C-c p" . cape-prefix-map)
;;   :config
;;   ;; eglot
;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; hippie-expand 涵盖了 dabbrev 的功能，但 corfu 也可以替代 hippie-expand 从而直接使用 dabbrev

;; (use-package hotfuzz-module
;;   :straight nil
;;   :load-path "~/.config/emacs/site-lisp/hotfuzz-module"
;;   :init
;;   ;; 手动加载共享库
;;   (require 'hotfuzz-module)
;;   :config
;;   (setq consult--tofu-char #x100000
;;         consult--tofu-range #x00fffe))

;; (use-package hotfuzz
;;   :straight t)

;; (use-package flx-rs
;;   :straight (flx-rs
;;              :repo "jcs-elpa/flx-rs"
;;              :fetcher github
;;              :files (:defaults "bin"))
;;   :unless kaladin/lsp-bridge
;;   :config
;;   (setq fussy-score-fn 'fussy-flx-rs-score)
;;   (flx-rs-load-dyn))
;;
;; (use-package fussy
;;   :straight t
;;   :unless kaladin/lsp-bridge
;;   :config
;;   (setq fussy-filter-fn 'fussy-filter-default)
;;   (setq fussy-score-fn 'fussy-flx-rs-score)
;;   (setq fussy-use-cache t)
;;   (setq completion-styles '(fussy basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles basic partial-completion))
;;                                         (eglot (styles fussy basic))
;;                                         (eglot-capf (styles fussy basic))))
;;   (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)
;;   (fussy-eglot-setup)
;;   (fussy-setup)
;;
;;   (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache)
;;   (add-hook 'corfu-mode-hook
;;             (lambda ()
;;               (setq-local fussy-max-candidate-limit 5000
;;                           fussy-default-regex-fn 'fussy-pattern-first-letter
;;                           fussy-prefer-prefix nil))))
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
