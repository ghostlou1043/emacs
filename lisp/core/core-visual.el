;;; core-visual.el --- Core-Visual -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-for-vline)
  ;; visual-fill-column-toggle-center-text 与 visual-line-mode 需要设置快捷键
  :config
  (setq visual-fill-column-center-text nil)
  (setq visual-fill-column-fringes-outside-margins t)
  (setq visual-fill-column-enable-sensible-window-split t)
  ;; (setq visual-fill-column-extra-text-width '(10.10)) 好像用不到
  
  (setq visual-fill-column-adjust-for-text-scale t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  
  (setq visual-fill-column-width nil))

(use-package lin  ;; 待配置定制
  :ensure t
  :config
  (setq lin-face 'lin-red)
  (setq lin-mode-hooks
        '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode-hook
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          pdf-outline-buffer-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  ;; (set-face-attribute 'hl-line nil :inherit 'lin-custom)
  ;; (set-face-attribute 'vertico-current nil :inherit 'lin-custom)
  (lin-global-mode 1))

(use-package pulsar ;; pulsar-highlight-permanently-dwim 待与其他包集成
  :ensure t
  :init
  (pulsar-global-mode 1)
  :hook ((next-error . pulsar-pulse-line)
         (minibuffer-setup . pulsar-pulse-line)
         (consult-after-jump . pulsar-recenter-top)
         (consult-after-jump . pulsar-reveal-entry))
  :config
  (setq pulsar-delay 0.066)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-red)
  (setq pulsar-region-face 'pulsar-red)
  (setq pulsar-highlight-face 'pulsar-red))

;; (use-package symbol-overlay
;;   :ensure t
;;   :bind
;;   ((:map global-map
;;          ("C-c o i" . symbol-overlay-put)
;;          ("C-c o f" . symbol-overlay-switch-forward)
;;          ("C-c o b" . symbol-overlay-switch-backward)
;;          ("C-c o r" . symbol-overlay-remove-all)
;;          ("C-c o v" . symbol-overlay-query-replace))
;;    (:map symbol-overlay-map
;;          ("f" . symbol-overlay-switch-forward)
;;          ("b" . symbol-overlay-switch-backward)
;;          )))


(provide 'core-visual)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-visual.el ends here





