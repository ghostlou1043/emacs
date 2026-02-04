;;; post-init.el --- Post-Init -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Version Check.
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "28.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Add load-path.
(defun update-load-path (&optional dirs)
  "Add a list of directories to the top of `load-path`.
If DIRS is provided, it should be a list of strings.
If DIRS is omitted (nil), it defaults to '(\"lisp\" \"os-lisp\" \"site-lisp\")."
  (let ((dirs-to-add (or dirs '("lisp" "os-lisp" "site-lisp"))))
    (dolist (dir dirs-to-add)
      (let ((full-path (expand-file-name dir user-emacs-directory)))
        (if (file-directory-p full-path)
            (unless (member full-path load-path)
              (push full-path load-path))
          (message "Directory does not exist, skipping load: %s" full-path))))))

(update-load-path '("lisp" "os-lisp" "site-lisp" "lisp/core"))

;; User Custom
(load custom-file 'noerror 'no-message)

;; Personalized customization.
(require 'init-1043)
(require 'init-lou)
(require 'init-ghost)

;; 是否使用 desktop 恢复 Buffer 与布局
(setq 1043/desktop t)

;; 是否使用 tab-bar
(setq 1043/tab-bar t)

;; 是否启用涉及 authinfo 相关插件
(setq 1043/authinfo t)

;; Network Proxy.
(setq lou/http-proxy-url "127.0.0.1")
(setq lou/http-proxy-port "7897")

(setq lou/https-proxy-url "127.0.0.1")
(setq lou/https-proxy-port "7897")

(setq lou/socks-proxy-url "127.0.0.1")
(setq lou/socks-proxy-port "7897")
(setq lou/socks-proxy-version "5")

;; Select Module.
;; (setq 1043/completion-backend 'lsp-bridge)
(setq 1043/completion-backend 'lsp-proxy)
;; (setq 1043/completion-backend 'eglot)

(setq 1043/file-manager 'dired)
(setq 1043/themes 'modus-themes)

;; Load built-in features and core configuration.
(require 'init-core)
(require 'init-corfu)
(require 'init-lsp-bridge)
(require 'init-lsp-proxy)
(require 'init-apheleia)
(require 'init-flycheck)
(require 'init-org-modern)
(require 'init-en)

(require 'init-os)


;; ;; 英文


;; (require 'init-misc)


;; ;; 编辑
;; (require 'init-edit)
;; (require 'init-meow)
;;
;; ;; ;; 文件管理
;;
;; ;; ;; 布局
;; (require 'init-windows)
;; (require 'init-dashboard)
;;

;; (require 'init-eaf)
;; (require 'init-paw)
;;
;; (require 'init-telega)
;; (require 'init-elfeed)
;;
;; (require 'init-treemacs)

;; 项目管理
;;(require 'init-project)

;;(require 'init-go)
;;(require 'init-python)

;;(require 'init-rust)

;; 笔记
;;(require 'init-org)
;;(require 'init-denote)

;; 各种 APP




;; Server Start.

(unless (daemonp)
  (require 'server)
  (unless (server-running-p)
    (if (boundp 'elpaca-after-init-hook)
        (add-hook 'elpaca-after-init-hook #'server-start)
      (add-hook 'after-init-hook #'server-start))))








;; word-wrap t： 启用单词换行，即在空白处换行而不是单词中间。
;; truncate-lines t： 默认启用行截断（不自动换行）。这与 word-wrap t 结合使用，意味着如果一行太长，它会被截断，但如果你手动启用 visual-line-mode，它会在单词边界换行。


;; 如何利用 Emacs 的帮助系统高效学习和使用 Emacs。
;; 如何通过 Ediff 进行高效的代码审查和合并。

;; (use-package compile-angel
;;   :straight t
;;   :demand t
;;   :custom
;;   ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
;;   ;; Drawback: The minibuffer will not display compile-angel's actions.
;;   (compile-angel-verbose t)
;;
;;   :config
;;   ;; The following directive prevents compile-angel from compiling your init
;;   ;; files. If you choose to remove this push to `compile-angel-excluded-files'
;;   ;; and compile your pre/post-init files, ensure you understand the
;;   ;; implications and thoroughly test your code. For example, if you're using
;;   ;; the `use-package' macro, you'll need to explicitly add:
;;   ;; (eval-when-compile (require 'use-package))
;;   ;; at the top of your init file.
;;   (push "/init.el" compile-angel-excluded-files)
;;   (push "/early-init.el" compile-angel-excluded-files)
;;   (push "/pre-init.el" compile-angel-excluded-files)
;;   (push "/post-init.el" compile-angel-excluded-files)
;;   (push "/pre-early-init.el" compile-angel-excluded-files)
;;   (push "/post-early-init.el" compile-angel-excluded-files)
;;
;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;
;;   ;; A global mode that compiles .el files prior to loading them via `load' or
;;   ;; `require'. Additionally, it compiles all packages that were loaded before
;;   ;; the mode `compile-angel-on-load-mode' was activated.
;;   (compile-angel-on-load-mode 1))


;; Temporary storage here
;; PROMPT: "Generate an optimized Emacs init.el using straight.el and the Minad stack.
;; Include Eglot (with Tinymist auto-export), Tempel (with collection), Magit,
;; Apheleia (with Ruff), Expand-region, Treemacs (bound to M-0 with follow-mode),
;; and esup. Support R, Python, Rmd/Qmd (via polymode + quarto-mode + poly-markdown),
;; SQL (sql-indent), YAML, and Typst (typst-ts-mode + treesit-auto + typst-preview).
;; UI: Catppuccin Latte, Doom-modeline, Nerd-icons, JetBrainsMono Nerd Font (height 150),
;; and org-modern. Optimization: 0.4s boot via deferred loading, 50MB startup GC,
;; DISABLE straight-check-for-modifications, and esup-depth 0. Features: short y/n,
;; custom.el file, delete-selection, electric-pair, column-number, pixel-scroll,
;; auto-fill for text, Vertico directory delete, consult-buffer with recentf
;; integration, suppress VC link prompts, DISABLE all backups/autosaves/lockfiles,
;; and fix Orderless matching to allow hyphen-based completion."

;; used https://opencode.ai/ + Claude Opus + https://github.com/keegancsmith/emacs-mcp-server . It's impressive at coding and debugging emacs.



;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; post-init.el ends here
