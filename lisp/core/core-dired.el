;;; core-dired.el --- Dired config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; ====================================================================
;;; Dired 核心配置
;;; ====================================================================

(use-package dired
  :ensure nil  ; Dired 是 Emacs 内置包，无需下载
  ;; :hook (dired-mode . dired-hide-details-mode) ;; [注意] 开启此项会导致 dired-subtree 展开时内容不可见，故禁用
  :bind
  ((:map global-map
         ("C-x d"   . dired)       ; 全局唤起 Dired
         ("C-x C-j" . dired-jump)) ; 跳转到当前文件所在的 Dired 目录

   (:map dired-mode-map
         ;; --- 导航 (Navigation) ---
         ("j" . dired-goto-file)        ; 跳转到文件
         ("n" . dired-next-line)        ; 下一行
         ("p" . dired-previous-line)    ; 上一行
         ("," . dired-up-directory)     ; 进入上级目录 (替代 ^)
         ("." . dired-view-file)        ; 查看文件 (只读模式)


         ;; --- 文件操作 (File Operations) ---
         ("a" . dired-find-alternate-file)    ; 在当前 buffer 打开文件 (不新建 buffer)
         ("o" . dired-find-file-other-window) ; 在另一窗口打开
         ("i" . dired-show-file-type)         ; 显示文件类型信息 (在 minibuffer)
         ("x" . dired-do-flagged-delete)      ; 执行标记的删除
         ("w" . dired-copy-filename-as-kill)  ; 复制文件名 (配合前缀参数可复制路径)
         ("W" . browse-url-of-dired-file)     ; 调用系统默认程序打开文件
         ("D" . dired-do-delete)              ; 删除标记的文件
         ("F" . dired-do-find-marked-files)   ; 打开所有标记的文件


         ;; --- 标记 (Marking) ---
         ("d" . dired-flag-file-deletion)   ; 标记为待删除 (按 x 执行)
         ("m" . dired-mark)             ; 标记
         ("u" . dired-unmark)           ; 取消标记
         ("U" . dired-unmark-all-marks) ; 取消所有标记
         ("t" . dired-toggle-marks)     ; 反转标记


         ;; --- 显示与视图 (Display & View) ---
         ("g" . revert-buffer)           ; 刷新
         ("l" . dired-do-redisplay)      ; 重绘当前行
         ("h" . dired-hide-details-mode) ; 隐藏/显示详细信息 (权限、大小等)
         ("s" . dired-sort-toggle-or-edit) ; 切换排序方式 (按时间/按名称)
         ("k" . dired-do-kill-lines)     ; 在显示中隐藏当前行 (不删除文件)

         ;; --- 高级/不常用功能 ---
         ("L" . dired-do-load)           ; 加载 Emacs Lisp 文件
         ("N" . dired-do-man)            ; 查看 Man 手册
         ("X" . dired-do-shell-command)  ; 运行 Shell 命令

         ;; --- Wdired (可编辑模式) ---
         ("C-x C-v" . wdired-change-to-wdired-mode))) ; 进入可写模式，像编辑文本一样重命名文件

  :config
  ;; -------------------------------------------------------------------
  ;; 1. 行为与安全设置 (Behavior & Safety)
  ;; -------------------------------------------------------------------
  (setq dired-dwim-target t                 ; 智能目标：如果分屏打开了两个 Dired，复制/移动时自动以另一个窗口为目标
        dired-deletion-confirmer 'y-or-n-p  ; 删除确认仅需按 'y' 或 'n'，不需要输入 'yes'
        dired-recursive-deletes 'top        ; 递归删除：'top 意味着只在删除非空目录时询问一次
        dired-recursive-copies 'always      ; 递归复制：总是递归复制目录，不询问
        dired-clean-confirm-killing-deleted-buffers nil ; 删除目录时不提示 kill 对应的 buffer
        dired-kill-when-opening-new-dired-buffer t      ; [性能/整洁] 进入新目录时 kill 掉旧 buffer，防止 buffer 爆炸
        dired-copy-preserve-time t)        ; 复制文件是否保存时间戳

  ;; -------------------------------------------------------------------
  ;; 2. 显示与外观 (Display)
  ;; -------------------------------------------------------------------
  (setq dired-ls-F-marks-symlinks nil       ; 不在符号链接文件名后添加 '@' 等标记
        dired-hide-details-hide-symlink-targets t   ; 开启 hide-details-mode 时隐藏软链目标
        dired-hide-details-hide-information-lines t ; 开启 hide-details-mode 时隐藏头部统计行
        dired-clean-up-buffers-too t)       ; 删除文件时同时删除对应的打开 buffer

  ;; -------------------------------------------------------------------
  ;; 3. 性能优化 (Performance)
  ;; -------------------------------------------------------------------
  (setq dired-free-space nil                ; [重要] 不查询剩余磁盘空间，极大提升远程 (TRAMP) 浏览速度
        auto-revert-remote-files nil        ; [重要] 不自动刷新远程文件，避免卡顿
        dired-auto-revert-buffer 'dired-buffer-stale-p) ; 仅在 buffer 过期时刷新，配合上一条使用

  ;; -------------------------------------------------------------------
  ;; 4. 系统兼容性与 ls 参数 (System Compatibility)
  ;; -------------------------------------------------------------------

  ;; 仅在没有 ls 可用的情况下生效
  (setq ls-lisp-verbosity nil) ; 隐藏文件的“所有者”和“所属组”这两列信息
  (setq ls-lisp-dirs-first t)  ; 目录（文件夹）排在文件前面

  ;; 默认 ls 参数：长格式、几乎所有文件、人类可读大小、目录排在前面
  (setq dired-listing-switches "-alh --group-directories-first")
  (setq dired-maybe-use-globstar t) ; 允许在 Shell 命令中使用 ** 通配符

  ;; 针对 macOS 的特殊处理 (依赖用户自定义变量 sys/macp)
  (when (bound-and-true-p sys/macp)
    (if (executable-find "gls")
        ;; 情况 A: 安装了 coreutils (brew install coreutils)
        (progn
          (setq insert-directory-program "gls") ; 使用 GNU ls
          (setq ls-lisp-use-insert-directory-program t))
      ;; 情况 B: 使用 macOS 原生 BSD ls
      (progn
        (setq dired-use-ls-dired nil) ; BSD ls 不支持 --dired，必须禁用以防报错
        ;; BSD ls 不支持 --group-directories-first，需移除该参数
        (setq dired-listing-switches "-alh"))))

  ;; -------------------------------------------------------------------
  ;; 5. Wdired 设置 (可写 Dired)
  ;; -------------------------------------------------------------------
  (setq wdired-create-parent-directories t  ; 重命名文件到不存在的目录时自动创建目录
        wdired-allow-to-change-permissions t)) ; 允许通过编辑权限位来修改文件权限

;;; ====================================================================
;;; Dired 扩展功能 (Extensions)
;;; ====================================================================

;; 1. Dired-X: 扩展功能 (忽略文件、猜测命令等)
(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode) ; 默认开启“忽略文件”模式，隐藏干扰文件
  :config
  (setq dired-omit-verbose nil) ; 忽略文件时不显示提示信息
  ;; (add-to-list 'dired-omit-extensions ".log") ;; 默认值已经很丰富，以后可以补充
  ;; 优化正则表达式的可读性
  (setq dired-omit-files
        (concat "\\`[.]\\'"                    ; 当前目录 .
                "\\|\\(?:\\.js\\)?\\.meta\\'"  ; .meta 文件
                "\\|\\.\\(?:elc\\|a\\|o\\|pyc\\|pyo\\|swp\\|class\\)\\'" ; 编译产物
                "\\|^\\.DS_Store\\'"           ; macOS 垃圾文件
                "\\|^\\.\\(?:svn\\|git\\)\\'"  ; 版本控制目录
                "\\|^\\.ccls-cache\\'"         ; LSP 缓存
                "\\|^__pycache__\\'"           ; Python 缓存
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^RCS$\\|,v$"
                "\\|^INDEX$\\|-t\\.tex$"
                "\\|^flycheck_.*"              ; Flycheck 临时文件
                "\\|^flymake_.*"))             ; Flymake 临时文件

  ;; 根据系统类型猜测打开文件的命令
  (let ((cmd (cond ((bound-and-true-p sys/mac-x-p) "open")
                   ((bound-and-true-p sys/linux-x-p) "xdg-open")
                   ((bound-and-true-p sys/nixos-x-p) "xdg-open")
                   ((bound-and-true-p sys/win32p) "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

;; 2. Dired-Aux: 辅助功能 (压缩、搜索、权限)
(use-package dired-aux
  :ensure nil
  :bind (:map dired-mode-map
              ("e" . dired-do-open)        ; 使用系统默认关联程序打开
              ("y" . dired-do-symlink)     ; 创建符号链接
              ("Y" . dired-do-relsymlink)  ; 创建相对路径符号链接
              ("z" . dired-do-compress-to) ; 压缩到...
              ("Z" . dired-do-compress)    ; 解压/压缩当前文件
              ("r" . dired-do-find-regexp) ; 在标记文件中搜索正则

              ("A" . dired-create-empty-file) ; 创建空文件
              ("C" . dired-do-copy)        ; 复制
              ("R" . dired-do-rename)      ; 重命名/移动
              ("G" . dired-do-chgrp)       ; 修改用户组
              ("M" . dired-do-chmod)       ; 修改权限 (chmod)
              ("O" . dired-do-chown)       ; 修改所有者
              ("T" . dired-do-touch)       ; 修改时间戳
              ("H" . dired-do-hardlink)    ; 硬链接
              ("I" . dired-maybe-insert-subdir) ; 插入子目录到当前 buffer
              ("Q" . dired-do-find-regexp-and-replace)) ; 正则替换
  :config
  (setq dired-create-destination-dirs 'ask) ; 复制/移动时如果目标目录不存在，询问是否创建
  (setq dired-vc-rename-file t)      ; 如果文件受版本控制，使用 git mv 等命令重命名
  (setq dired-isearch-filenames 'dwim)) ; C-s 搜索时，只匹配文件名 (除非匹配不到才匹配其余信息)

;; 3. Diredfl: 增强的高亮显示
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; 4. Treemacs Icons: 显示文件图标
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; 5. Dired Quick Sort: 快速排序菜单
(use-package dired-quick-sort
  :ensure t
  :config
  (dired-quick-sort-setup)) ; 默认绑定到 "S"

;; 6. Dired Subtree: 像目录树一样展开子目录
(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle))) ; 使用 TAB 键展开/折叠目录

;; 7. Dired Narrow: 实时过滤显示文件
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("K" . dired-narrow))) ; 按 K 进入过滤模式

;; 8. Dired Ranger: 模拟 Ranger 文件管理器的操作 (多选复制粘贴)
(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("b" . dired-ranger-bookmark)       ; 记录目录到书签
              ("B" . dired-ranger-bookmark-visit) ; 访问目录书签
              ("c" . dired-ranger-copy)           ; 添加到剪贴板环 (Copy Ring)
              ("v" . dired-ranger-paste)          ; 粘贴
              ("V" . dired-ranger-copy-ring)))    ; 查看剪贴板环内容

;; 9. Dired Open: 扩展打开方式
(use-package dired-open
  :ensure t
  :config
  ;; 优先使用 dired-guess-shell-alist-user 定义的规则，其次尝试进入子目录
  (setq dired-open-functions
        '(dired-open-guess-shell-alist
          ;; dired-open-xdg
          dired-open-subdir)))

;; 10. Dired Filter: 强大的文件过滤功能
(use-package dired-filter
  :ensure t
  :hook (dired-mode . dired-filter-mode)
  :bind ((:map dired-mode-map
               ("/" . dired-filter-map)          ; 按 / 唤起过滤菜单
               (";" . dired-filter-negate))      ; 反转过滤逻辑

         (:map dired-filter-map
               ("/" . dired-filter-pop-all)      ; 清除所有过滤器
               ("p" . dired-filter-pop)          ; 撤销上一次过滤
               ("n" . dired-filter-by-name)      ; 按名称过滤
               ("r" . dired-filter-by-regexp)    ; 按正则过滤
               ("e" . dired-filter-by-extension) ; 按后缀过滤
               ("d" . dired-filter-by-directory) ; 只看目录
               ("f" . dired-filter-by-file)      ; 只看文件
               ("." . dired-filter-by-dot-files) ; 只看点文件
               ("o" . dired-filter-by-omit)      ; 配合 omit-mode
               ("g" . dired-filter-by-git-ignored) ; 过滤 git 忽略文件
               ("m" . dired-filter-by-mode)      ; 按文件模式（权限）过滤
               ("s" . dired-filter-by-symlink)   ; 按符号链接过滤
               ("x" . dired-filter-by-executable); 按可执行权限过滤

               ("c" . dired-filter-decompose))   ; 分解过滤器
         ;; 过滤分组视图的快捷键
         (:map dired-filter-group-mode-map
               ("<backtab>" . nil)
               ("<tab>"     . nil)
               ("C-c C-p" . dired-filter-group-backward-drawer)
               ("C-c C-n" . dired-filter-group-forward-drawer)))

  :config
  (setq dired-filter-revert 'never) ; 过滤时不自动 revert buffer
  (setq dired-filter-verbose nil)     ; 是否显示过滤规则的细节信息，熟练后可关闭

  ;; 定义过滤分组，用于 dired-filter-group-mode
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Git"       (directory . ".git") (file . ".gitignore"))
           ("Directory" (directory))
           ("PDF"       (extension . "pdf"))
           ("LaTeX"     (extension "tex" "bib"))
           ("Source"    (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css" "java" "go" "rs"))
           ("Doc"       (extension "md" "rst" "txt" "org"))
           ("Archives"  (extension "zip" "rar" "gz" "bz2" "tar" "7z"))
           ("Images"    (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF"))))))

  ;; Show git info in dired
(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              ("'" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
(use-package dired-rsync
  :ensure t
  :bind (:map dired-mode-map
              ("P" . dired-rsync)))


(provide 'core-dired)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-dired.el ends here
