;;; core-themes.el --- Core-Themes -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :if (1043/enable-built-in-themes-p)
  :init
  (require-theme 'modus-themes) ; `require-theme' is ONLY for the built-in Modus themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  ;; Load the theme of your choice.
  (modus-themes-load-theme 'modus-vivendi-tinted)
  (define-key global-map (kbd "<f7>") #'modus-themes-toggle))

(use-package modus-themes
  :ensure t
  :if (1043/enable-modus-themes-p)
  :config
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi-tinted))
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-disable-other-themes t
        modus-themes-italic-constructs nil ;; Use italics for code syntax highlighting and related.
        modus-themes-bold-constructs nil ;; Use bold for code syntax highlighting and related.
        modus-themes-mixed-fonts t ;; Toggle the use of monospaced fonts for spacing-sensitive constructs (affects font families).
        modus-themes-prompts nil

        ;; Option for completion framework aesthetics 待补全配置后再进行配置
        ;; This affects Company, Corfu, Flx, Icomplete/Fido, Ido, Ivy, Orderless, Vertico, and the standard *Completions* buffer.
        modus-themes-completions nil ;; When its properties are nil or an empty list, matching characters in the user interface will have a bold weight and a colored foreground.

        ;; Toggle the use of proportionately spaced (variable-pitch) fonts in the User Interface.
        modus-themes-variable-pitch-ui nil
        )
        ;; Option for the headings’ overall style 待配置 org 与 org-agenda 后再配置  考虑终端
  (setq modus-themes-headings
        '((t . t)))           ; keep the default style

  (setq modus-themes-common-palette-overrides
        `((1043/red "#d00000")
          (cursor 1043/red)               ;; 终端中的光标颜色问题
          ;; (fg-space "#d00000")
          ;; (border "#d00000")
          ;;
          (comment fg-dim)
          ;; Make links use subtle
          (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)
          ;; Make the fringe invisible
          (fringe unspecified)

          ;; Make TODO and DONE more or less intense
          ;; Make headings more or less colorful
          ;; Make Org block colors more or less colorful
          (bg-prose-block-contents unspecified)  ;; 代码块背景色
          (bg-prose-block-delimiter unspeficied) ;; 代码块#+行前景色
          (fg-prose-block-delimiter fg-dim)      ;; 代码块#+行背景色

          (fg-line-number-inactive fg-dim)
          (fg-line-number-active 1043/red)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          ;; Add support for meow-mode

          ;; Make Org agenda more or less colorful  8.12-13
          ;; Make inline code in prose use alternative styles

          ;; Make mail citations and headers more or less colorful
          ;; Make the region preserve text colors, plus other styles
          ;; Make language underlines less colorful

          ;; From the section "Make the mode line borderless"
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
          (bg-mode-line-active unspecified)
          (bg-mode-line-inactive unspecified)

  ;;         ;; From the section "Make matching parenthesis more or less intense"
  ;;         ;; (bg-paren-match bg-magenta-intense)
  ;;         ;; (underline-paren-match fg-main))
        ))

  ;; (setq modus-operandi-palette-user
  ;;       '((1043/red "#d00000")))
  ;; (setq modus-vivendi-tinted-palette-user
  ;;       '((1043/red "#d00000")))
  ;; (setq modus-operandi-palette-overrides
  ;;       '((comment fg-dim)
  ;;         (cursor 1043/red)))
  ;; (setq modus-vivendi-tinted-palette-overrides
  ;;       '((comment fg-dim)
  ;;         (cursor 1043/red)))

  ;; Load the theme of your choice.
  ;; 确保在用户选择加载 Modus 主题时禁用其他主题 此功能会调用钩子 modus-themes-after-load-theme-hook
  (modus-themes-load-theme 'modus-vivendi-tinted)
  ;; 不要使用 use-package 的 :bind 以免 defer
  (define-key global-map (kbd "<f7>") #'modus-themes-toggle))

;; 终端中 window 之间的分隔线被隐藏，可开启行号模式作为分隔
(use-package spacious-padding
  :ensure t
  :config
  ;; These are the default values, but I keep them here for visibility.
  ;; Also check `spacious-padding-subtle-frame-lines'.
  (setq spacious-padding-widths
        '( :header-line-width 0
           :internal-border-width 0
           :mode-line-width 0
           :custom-button-width 0
           :tab-width 0
           :right-divider-width 1
           :fringe-width 8
           :scroll-bar-width 16))
  (setq spacious-padding-subtle-frame-lines
        '(:mode-line-active "#d00000" :mode-line-inactive spacious-padding-line-inactive))
  (spacious-padding-mode +1))

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

(provide 'core-themes)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-themes.el ends here
