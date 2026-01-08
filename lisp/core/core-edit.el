;;; core-edit.el --- Core Edit -*- lexical-binding: t -*-

;;; Commentary:
;; 修改 Emacs 内置的与编辑相关的选项
;; 启用或禁止编辑相关 mode
;; 配置与编辑相关的 Emacs 包
;; Modify built-in Emacs editing-related options
;; Enable or disable editing-related modes
;; Configure Emacs packages related to editing

;;; Code:
(use-package emacs
  :ensure nil
  :bind
  ((:map global-map
              ("C-e" . move-end-of-line)
              ("C-l" . recenter-top-bottom)
              ("C-x h" . mark-whole-buffer)

              ("C-d" . kill-word)
              ("M-d" . backward-kill-word)

              ("C-t" . transpose-chars)
              ("M-t" . transpose-words)
              ("C-x C-t" . transpose-lines)

              ("C-v" . scroll-up-command)
              ("M-v" . scroll-down-command)
              
              ("C-;" . comment-set-column)
              ("M-;" . comment-dwim)
              ("C-x C-;" . comment-line)

              ;; "IO" auto input BUG
              ;; ("M-[" . forward-paragraph)  
              ;; ("M-]" . backward-paragraph)
              
              ("M-b" . backward-word)
              ("M-f" . forward-word)

              ("M-=" . what-cursor-position)
              ("M-'" . hippie-expand)
              ("M-c" . capitalize-dwim)
              ("M-u" . upcase-dwim)
              ("M-l" . downcase-dwim)
              ("M-\\" . move-to-window-line-top-bottom)
              
              ("C-x -" . shrink-window-if-larger-than-buffer)
              ("C-x =" . balance-windows)
              ("C-x +" . balance-windows)
              
              ("C-c -" . shrink-window-if-larger-than-buffer)
              ("C-c =" . balance-windows)
              ("C-c +" . balance-windows)
              
              ("C-x C--" . text-scale-adjust)
              ("C-x C-=" . text-scale-adjust)
              ("C-x C-+" . text-scale-adjust)
              ("C-c C--" . text-scale-adjust)
              ("C-c C-=" . text-scale-adjust)
              ("C-c C-+" . text-scale-adjust)
              ))
  :config
  (repeat-mode +1)
  (delete-selection-mode +1)
  ;; Covered the relevant configuration in init.el
  ;; 64mb.
  (setq undo-limit 67108864)
  ;; 96mb.
  (setq undo-strong-limit 100663296)
  ;; 480mb.
  (setq undo-outer-limit 503316480))

(use-package vundo
  :ensure t
  :bind
  (:map global-map
        ("C-x u" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu
  :ensure t
  :bind
  (:map global-map
        ("C-/" . undo-fu-only-undo)
        ("M-/" . undo-fu-only-redo))
  :config
  (setq undo-fu-allow-undo-in-region t))

(use-package undo-fu-session
  :ensure t
  :config
  ;; (setq undo-fu-session-directory "") ;; FIX ME or Keep Default.
  (setq undo-fu-session-linear nil)
  (setq undo-fu-session-compression 'gz)
  (setq undo-fu-session-file-limit 100)

  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (setq undo-fu-session-incompatible-major-modes  ;; FIX ME
        '(dashboard-mode term-mode vterm-mode shell-mode eshell-mode comint-mode))

  (setq undo-fu-session-ignore-encrypted-files t)
  (setq undo-fu-session-ignore-temp-files t)

  (setq undo-fu-session-temp-directories '("/tmp" "/dev/shm")) ;; FIX ME

  (undo-fu-session-global-mode +1))

(use-package crux
  :ensure t
  :bind
  (:map global-map
        ("C-k" . lou/crux-smart-kill-line)
        ("C-a" . crux-move-beginning-of-line)
        ("C-'" . crux-duplicate-current-line-or-region)
        ("C-x C-d" . crux-delete-file-and-buffer)
        ("C-x C-r" . crux-rename-buffer-and-file)
        ("C-c C-;" . crux-duplicate-and-comment-current-line-or-region)
        ("M-j" . crux-top-join-line)

        ;; ("M-k" . crux-kill-other-buffers)
        ;; crux-open-with
        ;; crux-indent-rigidly-and-copy-to-clipboard
        ;; crux-find-user-init-file
        ;; crux-find-user-custom-file
        ;; crux-find-shell-init-file
        ;; crux-find-current-directory-dir-locals-file
        ;; crux-reopen-as-root

        ;; ("C-x K" . crux-kill-other-buffers)
        ;; ("C-x C-r" . crux-reopen-as-root-mode)  ; 以 root 权限重新打开文件
        ;; ("C-c C-d" . crux-downcase-region)      ; 区域转小写
        ;; ("C-c C-u" . crux-upcase-region)        ; 区域转大写
        ;; ("C-c C-t" . crux-transpose-windows)      ; 交换窗口
        ;;
        ;; ("C-c C-f" . crux-recentf-find-file)      ; 快速打开最近的文件
        ;; ;; crux-recentf-find-directory
        ;; ("C-c C-y" . crux-copy-file-preserve-attributes) ; 复制文件保留属性
        ;; ("C-c C-e" . crux-eval-and-replace)     ; 评估 Elisp 表达式并替换
        )

  :config
  (defun lou/crux-smart-kill-line ()
    "如果光标在行首，强制删除整行；否则使用 crux 智能删除。"
    (interactive)
    (if (bolp)
        (kill-whole-line)
      (crux-smart-kill-line)))

  ;; 待研究，使用对应的命令将应用与 region or buffer
  (crux-with-region-or-buffer indent-region) ;; 这个函数用于对选中的区域进行缩进，如果没有选中区域，则会缩进整个缓冲区的内容
  (crux-with-region-or-buffer untabify) ;; 这个函数用于将选中的区域或整个缓冲区中的制表符（tab）转换为空格
  ;; C-w
  (crux-with-region-or-sexp-or-line kill-region)
  ;; M-w
  (crux-with-region-or-point-to-eol kill-ring-save))

(use-package smartparens
  :ensure t
  :hook
  ((prog-mode . smartparens-mode)  ;; FIX ME
   (text-mode . smartparens-mode))
  :bind
  ((:map smartparens-mode-map      ;; FIX Repeat.
         ("M-a" . sp-backward-sexp)
         ("M-e" . sp-forward-sexp)

         ("C-c C-a" . sp-beginning-of-sexp)
         ("C-c C-e" . sp-end-of-sexp)

         ("C-c C-b" . sp-backward-symbol)
         ("C-c C-f" . sp-forward-symbol)

         ("C-c C-s" . sp-transpose-sexp)

         ;; ("C-c C-i" . sp-change-enclosing) ;; 改变当前所在区域
         ;; ("C-x C-i" . sp-change-inner) ;; 改变离自己最近的一个小区域

         ;; 根据不同的前缀选择不同的东西，正负 arg 选方向+多少，C-u 选直到当前表达式结束，
         ;; C-u C-u 选表达式全部，0 选表达式内全部
         ;; 待研究
         ("C-c C-," . sp-select-next-thing-exchange) ;; 在结果会交换光标与 mark 的位置
         ("C-c C-." . sp-select-next-thing)

         ("C-c a" . sp-backward-barf-sexp) ;; 把表达式扔到左边
         ("C-c e" . sp-forward-barf-sexp) ;; 把表达式扔到右边

         ("C-c b" . sp-backward-slurp-sexp) ;; 反向吸收,把左边的表达式吸进来 ->
         ("C-c f" . sp-forward-slurp-sexp) ;; 正向吸收,把右边的表达式吸进来 <-

         ("C-c i f" . sp-down-sexp)
         ("C-c i b" . sp-backward-down-sexp) ;; 反向到往嵌套最深处

         ("C-c o f" . sp-up-sexp)
         ("C-c o b" . sp-backward-up-sexp) ;; 反向离开嵌套最深处

         ("C-M-b" . sp-splice-sexp-killing-backward) ;; 解掉括号，清除光标前(反向)部分
         ("C-M-f" . sp-splice-sexp-killing-forward) ;; 解掉括号，清除光标后(正向)部分

         ("C-<backspace>" . sp-kill-symbol)
         ("M-<backspace>" . sp-backward-kill-symbol)

         ;; ("C-M-s" . sp-splice-sexp) ;; 可根据 arg 解掉相距当前光标位置的第 arg 层括号
         ;; ("C-M-r" . sp-raise-sexp) ;; 提升表达式（也是解掉括号），并根据 arg 的正负保留前后 arg 个部分
         )
   (:repeat-map 1043/smartparens-down-repeat-map
                ("b" . sp-backward-down-sexp) ;; 反向到往嵌套最深处
                ("f" . sp-down-sexp))

   (:repeat-map 1043/smartparens-up-repeat-map
                ("b" . sp-backward-up-sexp) ;; 反向离开嵌套最深处
                ("f" . sp-up-sexp))

   (:repeat-map 1043/smartparens-backward-repeat-map
                ("M-a" . sp-backward-sexp)
                ("C-a" . sp-beginning-of-sexp)
                ("C-b" . sp-backward-symbol)
                ("a" . sp-backward-barf-sexp) ;; 把表达式扔到左边
                ("b" . sp-backward-slurp-sexp) ;; 反向吸收,把左边的表达式吸进来 ->
                )
   (:repeat-map 1043/smartparens-forward-repeat-map
                ("M-e" . sp-forward-sexp)
                ("C-e" . sp-end-of-sexp)
                ("C-f" . sp-forward-symbol)
                ("e" . sp-forward-barf-sexp) ;; 把表达式扔到右边
                ("f" . sp-forward-slurp-sexp) ;; 正向吸收,把右边的表达式吸进来 <-
                ))
  :config
  (require 'smartparens-config)  ;; 加载 smartparens 默认配置
  ;; 禁用 emacs 自带的括号匹配高亮显示
  (show-paren-mode -1)
  ;; 显示配对的括号
  (setq sp-show-pair-delay 0.125)
  (show-smartparens-global-mode +1))

(use-package anzu
  :ensure t
  :bind
  (:map global-map
        ("C-r" . anzu-query-replace)
        ("M-r" . anzu-query-replace-regexp))
  :config
  (setq anzu-mode-lighter "")
  (setq anzu-deactivate-region nil)
  (setq anzu-search-threshold 1000)  ;; 搜索结果上限为1000个
  (setq anzu-replace-threshold 1000)
  (setq anzu-replace-to-string-separator " => ")
  (global-anzu-mode +1))

(use-package expand-region
  :ensure t
  :init
  (defvar 1043/expand-region-map (make-sparse-keymap)
    "Keymap for `expand-region'.")
  (which-key-add-key-based-replacements "M-h" "expand-region")
  :bind-keymap
  ("M-h" . 1043/expand-region-map)
  :bind
  ((:map global-map
         ("M-m" . er/expand-region))
   (:map 1043/expand-region-map
         ("d" . er/mark-defun)
         (";" . er/mark-comment)
         ("a" . er/mark-next-accessor)
         ("m" . er/mark-method-call)

         ("u" . er/mark-url)
         ("s" . er/mark-sentence)
         ("p" . er/mark-paragraph)
         ("n" . er/mark-ts-node)
         ("h" . er/mark-html-attribute)

         ("o" . er/mark-org-parent)
         ("e" . er/mark-org-element)

         ("y" . er/mark-yaml-outer-block)
         ("i" . er/mark-yaml-inner-block)
         ("b" . er/mark-yaml-block)
         ("k" . er/mark-yaml-key-value)
         ("l" . er/mark-yaml-list-item)
         )))

(use-package multiple-cursors
  :ensure t
  :init
  ;; (let ((mc-file (expand-file-name "~/dotfiles/config/emacs/.mc-lists.el")))
  ;;   (when (file-exists-p mc-file)
  ;;     (setq mc/list-file mc-file)))
  (which-key-add-key-based-replacements "C-c m" "multiple-cursors")
  :bind-keymap
  ("C-c m" . mc/keymap)
  :bind
  ((:map global-map
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this))
   (:map mc/keymap
         (">" . mc/mark-next-like-this)
         ("<" . mc/mark-previous-like-this)
         ("f" . mc/mark-next-like-this-word)
         ("b" . mc/mark-previous-like-this-word)
         ("F" . mc/mark-next-like-this-symbol)
         ("B" . mc/mark-previous-like-this-symbol)

         ("m" . mc/mark-all-dwim)
         ("n" . mc/skip-to-next-like-this)
         ("p" . mc/skip-to-previous-like-this)
         ("," . mc/unmark-previous-like-this)
         ("." . mc/unmark-next-like-this)
         ("l" . mc/edit-lines)
         ("e" . mc/edit-ends-of-lines)
         ("a" . mc/edit-beginnings-of-lines)
         ;;
         ("r" . mc/mark-all-in-region)
         ("w" . mc/mark-all-words-like-this)
         ("s" . mc/mark-all-symbols-like-this)
         ("d" . mc/mark-all-like-this-in-defun)
         ("x" . mc/mark-more-like-this-extended)
         ;;
         ("i" . mc/insert-numbers)
         ("z" . mc/insert-letters)
         ("'" . mc/sort-regions)
         (";" . mc/reverse-regions))

   (:repeat-map 1043/multiple-cursors-repeat-map
                (">" . mc/mark-next-like-this)
                ("<" . mc/mark-previous-like-this)
                ("f" . mc/mark-next-like-this-word)
                ("b" . mc/mark-previous-like-this-word)
                ("F" . mc/mark-next-like-this-symbol)
                ("B" . mc/mark-previous-like-this-symbol)

                ("m" . mc/mark-all-dwim)
                ("n" . mc/skip-to-next-like-this)
                ("p" . mc/skip-to-previous-like-this)
                ("," . mc/unmark-previous-like-this)
                ("." . mc/unmark-next-like-this)
                :exit
                ("l" . mc/edit-lines)
                ("e" . mc/edit-ends-of-lines)
                ("a" . mc/edit-beginnings-of-lines)
                ;;
                ("r" . mc/mark-all-in-region)
                ("w" . mc/mark-all-words-like-this)
                ("s" . mc/mark-all-symbols-like-this)
                ("d" . mc/mark-all-like-this-in-defun)
                ("x" . mc/mark-more-like-this-extended)
                ;;
                ("i" . mc/insert-numbers)
                ("z" . mc/insert-letters)
                ("'" . mc/sort-regions)
                (";" . mc/reverse-regions)
                ))
  :config
  (setq mc/always-run-for-all t))


(use-package ace-window
  :ensure t
  :bind
  (:map global-map
        ("C-x o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))  ;; 设置窗口选择时的快捷键
  (setq aw-background t)  ;; 暗化其它窗口
  (setq aw-minibuffer-flag t))

(use-package avy
  :ensure t
  :bind
  (:map global-map
        ("C-j" . avy-goto-char-timer))
  :config
   ;; 只在当前窗口中跳转
  (setq avy-all-windows t)
   ;; 设置背景暗化，以便更容易看到avy标签
  (setq avy-background t))

(use-package ace-pinyin
  :ensure t
  :after avy
  :delight
  :config
  (setq ace-pinyin-treat-word-as-char t)
  (ace-pinyin-global-mode +1))

;; (use-package avy-zap
;;   :ensure t
;;   :bind
;;   (:map global-map
;;         ("C-c z" . avy-zap-up-to-char-dwim) ;; 只删除光标到目标字符之间的内容，不包括目标字符本身。
;;         ("C-c Z" . avy-zap-to-char-dwim))) ;; 包括目标字符本身。

;; 使用 treesit-fold 替代
;; https://github.com/emacs-tree-sitter/treesit-fold
;; (use-package origami
;;   :straight t
;;   :bind
;;   (:map origami-mode-map
;;         ("C-r t" . origami-toggle-node)
;;         ("C-r q" . origami-recursively-toggle-node)
;;         ("C-r g" . origami-forward-toggle-node)
;;         ("C-r r" . origami-reset)
;;         ("C-r /" . origami-undo)
;;         ("C-r z" . origami-redo)
;;         ("C-r x" . origami-show-only-node)
;;         ("C-r [" . origami-previous-fold)
;;         ("C-r ]" . origami-next-fold)
;;         ("C-r \\" . origami-forward-fold)
;;         ("C-r ," . origami-backward-fold-same-level)
;;         ("C-r ." . origami-forward-fold-same-level)
;;         ("C-r v" . origami-show-node)
;;         ("C-r f" . origami-open-node-recursively)
;;         ("C-r j" . origami-open-node-recursively)
;;         ("C-r ;" . origami-open-all-nodes)
;;         ("C-r '" . origami-close-all-nodes)
;;         )
;;   :config
;;   (global-origami-mode +1))



(provide 'core-edit)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-edit.el ends here
