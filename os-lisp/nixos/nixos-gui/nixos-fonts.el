;;; nixos-fonts.el --- NixOS-Fonts -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package show-font
  :ensure t)

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
           :default-height 105
           )
          (medium
           :default-height 120
           )
          (regular
           :default-height 150    ;; DPI96，22磅，磅数为偶数方可中英文等宽
           )
          (large
           :default-height 180
           )
          (huge
           :default-height 195
           )
          (gigantic
           :default-height 210
           )
          (immense
           :default-height 225
           )
          (colossal
           :default-height 240
           )
          (t
           :default-family "IosevkaTerm Nerd Font Mono"
           :default-weight regular
           :default-height 150    ;; DPI96，22磅，磅数为偶数方可中英文等宽启动
           :default-slant normal
           :default-width normal

           :fixed-pitch-family "IosevkaTerm Nerd Font Mono"
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0

           :variable-pitch-family "IosevkaTermSlab Nerd Font"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0

           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           
           :line-spacing nil)))
  (when (fboundp '1043/redraw-emoji-mono)
    (add-hook 'fontaine-set-preset-hook #'1043/redraw-emoji-mono)) ;; 令 emoji 随着字体放大也能等宽
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode +1))

(defun sys/set-fonts ()
  (1043/set-font-family-list)
  ;; Fontaine
  (when (locate-library "fontaine")
    (fontaine-mode +1))
  ;; Latin
  (1043/set-font-if-exists 'latin "IosevkaTerm Nerd Font")
  ;; Greek
  (1043/set-font-if-exists 'greek "IosevkaTerm Nerd Font")
  ;; Emoji
  (1043/set-font-if-exists 'emoji "Noto Color Emoji")
  ;; (setq default-text-properties '(line-spacing 0.1 line-height 1.1))
    ;; Symbols
  (1043/set-font-if-exists 'symbol "IosevkaTerm Nerd Font")
  ;; Symbols append
  (when (member "Symbols Nerd Font" 1043/font-family-list)
    (set-fontset-font t 'symbol (font-spec :family "Symbols Nerd Font") nil 'append))
  (when (member "Noto Sans Symbols" 1043/font-family-list)    
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'append))
  (when (member "Noto Sans Symbols 2" 1043/font-family-list)
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'append))
  (when (member "Symbola" 1043/font-family-list)
    (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'append))
  
  ;; CJK
  (1043/set-font-if-exists 'han "LXGW WenKai")
  (1043/set-font-if-exists 'cjk-misc "LXGW WenKai")
  (1043/set-font-if-exists 'kana   "Noto Sans CJK JP")
  (1043/set-font-if-exists 'hangul "Noto Sans CJK KR")

  ;; Southeast Asia
  (1043/set-font-if-exists 'khmer    "Noto Sans Khmer")
  (1043/set-font-if-exists 'lao      "Noto Sans Lao")
  (1043/set-font-if-exists 'burmese  "Noto Sans Myanmar")
  (1043/set-font-if-exists 'thai     "Noto Sans Thai")

  ;; Middle East & Africa
  (1043/set-font-if-exists 'hebrew    "Noto Sans Hebrew")
  (1043/set-font-if-exists 'arabic    "Noto Sans Arabic")
  (1043/set-font-if-exists 'ethiopic  "Noto Sans Ethiopic")

  ;; Indic
  (1043/set-font-if-exists 'gujarati    "Noto Sans Gujarati")
  (1043/set-font-if-exists 'devanagari  "Noto Sans Devanagari")
  (1043/set-font-if-exists 'kannada     "Noto Sans Kannada")
  (1043/set-font-if-exists 'malayalam   "Noto Sans Malayalam")
  (1043/set-font-if-exists 'oriya       "Noto Sans Oriya")
  (1043/set-font-if-exists 'sinhala     "Noto Sans Sinhala")
  (1043/set-font-if-exists 'tamil       "Noto Sans Tamil")
  (1043/set-font-if-exists 'telugu      "Noto Sans Telugu")
  (1043/set-font-if-exists 'tibetan "Noto Serif Tibetan"))

(provide 'nixos-fonts)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; nixos-fonts.el ends here
