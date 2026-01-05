;;; core-save.el --- Core-Save -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (history-length 300)
  (savehist-save-minibuffer-history t)
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     compile-command                  ; 编译命令历史     
     file-name-history                ; 文件名历史
     minibuffer-history               ; minibuffer 历史
     query-replace-history            ; 查找替换历史
     shell-command-history            ; shell 命令历史          
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring))); 搜索记录

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 600))

(provide 'core-save)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-save.el ends here
