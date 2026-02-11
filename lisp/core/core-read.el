;;; core-init.el --- Read -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vlf
  :ensure t
  :config
  (setq vlf-tune-enabled t)
  (setq vlf-tune-max 100000000)
  ;; ;; 是否询问打开大文件
  (setq vlf-application 'dont-ask) ;; 'dont-ask / t / nil 可选
  ;; ;; 默认 chunk 大小（单位字节），可按需调整
  (setq vlf-batch-size (* 1 1024 1024))) ;; 1MB 分块

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))  ;; 设置 EPUB 文件的文本宽度

;; Emacs 中 wallabag 客户端
(use-package wallabag
  :ensure t
  :config
  ;; 图片缓存
  (setq url-automatic-caching t)
  ;; wallabag server host name
  ;; 务必使用配置中的域名，否则导致显示错误
  (setq wallabag-host "http://localhost:8080")
  
  ;; ;; username
  ;; (setq wallabag-username "wallabag")
  ;; ;; password
  ;; (setq wallabag-password "wallabag")
  ;; ;; created with API clients management
  ;; (setq wallabag-clientid "1_2krt2tql5tk440kks0ss4kkgkoc0o8o8co4swg40kgk44cgg4")
  ;; ;; created with API clients management
  ;; (setq wallabag-secret "385w287wg1yc48gsg0ko0gg0c0080o80044wkgccsgc808skso")

  (setq wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date")) ;; control what content should be show in *wallabag-search*
  (setq wallabag-search-page-max-rows 32) ;; how many items shown in one page
  ;; (setq wallabag-db-file "~/OneDrive/Org/wallabag.sqlite") ;; optional, default is saved to ~/.emacs.d/.cache/wallabag.sqlite
  )

(provide 'core-read)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-init.el ends here
