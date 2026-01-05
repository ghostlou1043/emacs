;;; core-recentf.el --- Core-Recentf -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" 
         "\\.bz2$" "\\.bz$" "\\.gz$" "\\.gzip$" 
         "\\.xz$" "\\.zip$" "\\.7z$" "\\.rar$"

         "^/tmp/"
         "^/var/folders/.+$"         
         "^/usr/include/"

         "/ssh:"  
         "/sudo:" 
         
         ".cask"
         "url"

         "\\.?cache"         
         "/.elfeed/"

         "/TAGS\\'"
         "/G?TAGS$"         
         "savehist"
         "bookmarks"         
         "saveplaces"
         "undo-tree-hist"
         
         "\\.revive$"
         "persp-confs"
         "/persp-confs/"
         "\\.?ido\\.last$"
         
         "autoload\\.el$"
         "-autoloads\\.el$"
         "\\(?:\\.emacs\\.d\\|emacs\\)/\\(?:elpa\\|straight\\|elpaca\\)"
         
         "COMMIT_EDITMSG\\'"
         
         "\\.\\(?:pdf\\|docx?\\|xlsx?\\)$"
         "\\.\\(?:gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"))
  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(provide 'core-recentf)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-recentf.el ends here
