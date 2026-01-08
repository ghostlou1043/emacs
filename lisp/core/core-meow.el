;;; core-meow.el --- Core Meow -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package meow
  :ensure t
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     ;; 屏蔽 ESC 按键
     '("<escape>" . ignore))

    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     ;; 即 C-u 0-9
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     ;; c h o m x 不可使用
     ;; 尽量为 Emacs 原生命令

     ;; '("a" . meow-back-word)
     ;; '("b" . meow-back-word)
     ;; '("c" . meow-back-word)
     ;; '("d" . daemons)
     ;; '("e" . meow-back-word)
     ;; '("f" . meow-back-word)
     ;; '("g" . gptel)
     ;; '("h" . meow-back-word)
     ;; '("i" . meow-back-word)
     ;; '("j" . meow-back-word)
     ;; '("k" . meow-back-word)
     ;; '("l" . meow-back-word)
     ;; '("m" . meow-back-word)
     ;; '("n" . meow-back-word)
     ;; '("o" . meow-back-word)
     ;; '("p" . meow-back-word)
     ;; '("q" . meow-back-word)
     ;; '("r" . meow-back-word)
     ;; '("s" . meow-back-word)
     ;; '("t" . meow-back-word)
     ;; '("u" . meow-back-word)
     ;; '("v" . meow-back-word)
     ;; '("w" . meow-back-word)
     ;; '("x" . meow-back-word)
     ;; '("y" . meow-back-word)
     ;; '("z" . meow-back-word)

     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)

     '("c" . "C-c")
     '("o" . "C-o")
     '("x" . "C-x")
     '("h" . "C-h")

     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("f" . meow-next-word)
     '("F" . meow-next-symbol)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)

     '("i" . meow-insert)
     '("a" . meow-append)

     '("u" . meow-open-above)
     '("n" . meow-open-below)
     '("e" . meow-goto-line)

     '("H" . meow-left-expand)
     '("J" . meow-next-expand)
     '("K" . meow-prev-expand)
     '("L" . meow-right-expand)

     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-line)

     '("m" . meow-change)
     '("p" . meow-pop-selection)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-mark-symbol)
     '("t" . meow-swap-grab)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("y" . meow-sync-grab)
     '("z" . meow-till)

     '("/" . meow-search)
     '("`" . meow-tree-sitter-node)
     '("-" . negative-argument)
     '("=" . meow-find)
     '(";" . meow-reverse)
     '("'" . repeat)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("<escape>" . ignore)))
  (meow-setup)

  ;; 快捷指南中的键盘支持 ANSI 和 ISO 布局
  (setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi)

  (setq meow-cursor-type-insert 'box)
  (custom-set-faces
   '(meow-insert-cursor ((t (:background "#FFFFFF"))))
   '(meow-normal-cursor ((t (:background "#d00000")))))

  (setq meow-expand-exclude-mode-list '(markdown-mode org-mode))
  (add-to-list 'meow-mode-state-list '((vundo-mode . insert)
                                       (fundamental-mode . motion)
                                       (dashboard-mode . insert)))
  (setq meow-keypad-self-insert-undefined nil)
  (setq meow-keypad-start-keys '((?c . ?c)
                                 (?h . ?h)
                                 (?x . ?x)
                                 (?o . ?o)))
  (setq meow-keypad-meta-prefix nil)
  (setq meow-keypad-ctrl-meta-prefix ?m)
  (setq meow-visit-sanitize-completion t)
  (setq meow-expand-hint-remove-delay 2)
  (setq meow-keypad-describe-delay 0)
  (setq meow-display-thing-help t)
  (setq meow-select-on-change t)
  (setq meow-use-clipboard t)
  (meow-global-mode +1))

(use-package meow-tree-sitter
  :ensure t
  :config
  (meow-tree-sitter-register-defaults))


(provide 'core-meow)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-meow.el ends here
