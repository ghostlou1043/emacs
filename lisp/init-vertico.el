;;; init-vertico.el --- Init Vertico -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico ;; minibuffer 补全 UI
  :ensure t
  :unless (1043/enable-lsp-bridge-p)

  :config
  (setq vertico-scroll-margin 2) ;; Different scroll margin
  (setq vertico-count 10) ;; Show more candidates
  (setq vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (setq vertico-cycle nil) ;; Enable cycling for `vertico-next/previous'

  ;; Prompt indicator for `completing-read-multiple'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

  ;; org-refile
  ;; Alternative 1: Use the basic completion style
  ;; (setq org-refile-use-outline-path 'file
  ;;       org-outline-path-complete-in-steps t)
  ;;
  ;; (advice-add #'org-olpath-completing-read :around #'vertico-enforce-basic-completion)
  ;;
  ;; (defun vertico-enforce-basic-completion (&rest args)
  ;;   (minibuffer-with-setup-hook
  ;;       (:append
  ;;        (lambda ()
  ;;          (let ((map (make-sparse-keymap)))
  ;;            (define-key map [tab] #'minibuffer-complete)
  ;;            (use-local-map (make-composed-keymap (list map) (current-local-map))))
  ;;          (setq-local completion-styles (cons 'basic completion-styles)
  ;;                      vertico-preselect 'prompt)))
  ;;     (apply args)))

  ;; Alternative 2: Complete full paths
  ;; 优点：这样就可以利用 orderless 等高级样式对整个路径进行模糊匹配，效率可能更高。
  ;; 缺点：补全列表可能会变得非常长和混乱，因为所有可能的完整路径都会被列出来。
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)


  ;; org-agenda-filter and org-tags-view
  (advice-add #'org-make-tags-matcher :around #'vertico-enforce-basic-completion)
  (advice-add #'org-agenda-filter :around #'vertico-enforce-basic-completion)

  (vertico-mode +1))



;; Configure directory extension.
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide 'init-vertico)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-vertico.el ends here
