;;; nixos-input.el --- NixOS Input. -*- lexical-binding: t -*-
;;; Commentary: 
;;; Code:

(use-package rime
  :ensure t
  :bind
  ((:map global-map
         ("C-|" . rime-mode)
         ("C-\\" . toggle-input-method))
   (:map rime-mode-map
         ("C-<f13>" . 'rime-force-enable)
         ("C-<XF86Tools>" . 'rime-force-enable)
         ;; ("<f13>" . 'rime-inline-ascii)
         ;; "C-`" 键绑定到发送按键绑定事件
         ("C-`" . 'rime-send-keybinding)))
  :custom
  ;; 设置默认输入法为 Rime 
  (default-input-method "rime")
  (rime-librime-root 1043/rime-librime-root)
  ;; 需要原始的 pkgs.emacs-gtk 或其他类似的路径，不能够是自定义的包的路径，会导致 include 文件夹找不到
  (rime-emacs-module-header-root (expand-file-name "include" (getenv "EMACS_PATH")))
  (rime-share-data-dir (getenv "FCITX5_RIME_DATA"))

  ;; MacOS
  ;; 指定 librime 库的位置
  ;; (rime-librime-root "~/.config/emacs/librime/dist")
  ;; 指定 Emacs 模块头文件的路径 FIX
  ;; (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@31/31.0.50/include")

  ;; 设置 Rime 用户数据的存储目录
  (rime-user-data-dir "~/.config/emacs/rime")
  ;; 设置 Rime 输入法的光标样式
  (rime-cursor "˰")
  ;; 使用 posframe 显示候选词
  (rime-show-candidate 'posframe)
  ;; (rime-show-candidate 'minibuffer) ;; 也可以选择使用 minibuffer 显示候选词
  ;; 禁用 Rime 的条件设置
  (rime-disable-predicates
   '(meow-normal-mode-p
     meow-motion-mode-p
     meow-keypad-mode-p
     rime-predicate-hydra-p               ;; 在 hydra 激活时禁用
     rime-predicate-evil-mode-p           ;; 在 evil-mode 中禁用 Rime
     rime-predicate-ace-window-p          ;; 在激活 ace-window 模式时禁用 Rime
     rime-predicate-prog-in-code-p        ;; 在 prog-mode 和 conf-mode 中的非注释或引号内禁用 Rime
     rime-predicate-after-ascii-char-p    ;; 在输入 ASCII 字符后禁用 Rime
     ;; rime-predicate-space-after-ascii-p   ;; 在英文字符后跟随空格时禁用 Rime
     rime-predicate-tex-math-or-command-p ;; 在 TeX 数学模式或命令中禁用 Rime
     rime-predicate-punctuation-after-ascii-p  ;; 在英文字符后输入标点符号时禁用 Rime
     rime-predicate-punctuation-after-space-cc-p ;; 在中文字符后跟随空格时输入标点符号时禁用 Rime
     rime-predicate-punctuation-after-space-en-p
     rime-predicate-punctuation-line-begin-p ;; 在行首输入标点符号时禁用 Rime
     rime-predicate-current-uppercase-letter-p)) ;; 在大写字母时禁用 Rime
  ;; 在 mode-line 显示当前输入法状态
  (mode-line-mule-info '((:eval (rime-lighter))))
  ;; 设置候选词框的 face 属性，调整颜色
  (set-face-attribute 'rime-default-face nil :foreground "#d830f2" :background 'unspecified)
  (set-face-attribute 'rime-code-face nil :foreground "#1bd672" :background 'unspecified)
  (set-face-attribute 'rime-candidate-num-face nil :foreground "#000000" :background 'unspecified)
  (set-face-attribute 'rime-comment-face nil :foreground "#000000" :background 'unspecified)
  ;; 设置 posframe 的显示属性
  (rime-posframe-properties
   (list :background-color nil            ;; 背景颜色为透明（可以设置其他颜色，如 "#2E3440"）
         :foreground-color nil            ;; 前景色为透明（可以设置其他颜色，如 "#D8DEE9"）
         ;; :font "Fira Code-18"          ;; 设置字体及大小
         :internal-border-width 1))       ;; 内边距为 1 像素
  :config
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "," "."))
  ;; 临时英文模式下阻止标点直接上屏，使用 "x" 作为占位符
  (setq rime-inline-ascii-holder ?x))


(provide 'nixos-input)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; nixos-input.el ends here
