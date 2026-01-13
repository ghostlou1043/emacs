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




(provide 'core-read)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-init.el ends here
