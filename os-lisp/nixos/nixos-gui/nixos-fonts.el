;;; nixos-fonts.el --- NixOS-Fonts -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package fontaine
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-<f6>" . fontaine-set-preset))
  :config
  (setq fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((small
           :default-height 120
           )
          (regular
           :default-height 165    ;; DPI96，22磅，磅数为偶数方可中英文等宽
           )
          (large
           :default-height 210
           )
          (t
           :default-family "Sarasa Fixed SC"
           :default-weight regular
           :default-slant normal
           :default-width normal

           :fixed-pitch-family "Sarasa Fixed SC"
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0

           :variable-pitch-family "Sarasa UI SC"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0

           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic

           :line-spacing nil)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode +1))

(provide 'nixos-fonts)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; nixos-fonts.el ends here
