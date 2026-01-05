;;; init-lou.el --- Init-Lou -*- lexical-binding: t; -*-
;;; Commentary: 个人使用实用函数
;;; Code:

;; Network
(defvar lou/http-proxy-url "127.0.0.1" "HTTP 代理 IP")
(defvar lou/http-proxy-port "7890"     "HTTP 代理端口 (通常用于非加密网页)")

(defvar lou/https-proxy-url "127.0.0.1" "HTTPS 代理 IP")
(defvar lou/https-proxy-port "7890"     "HTTPS 代理端口 (通常用于加密网页，如下载插件)")

(defvar lou/socks-proxy-url "127.0.0.1" "SOCKS 代理 IP")
(defvar lou/socks-proxy-port "7890"     "SOCKS 代理端口")
(defvar lou/socks-proxy-version "5"     "SOCKS 版本")

(defvar lou/check-proxy-ip-url "https://ipinfo.io/json")

(defvar lou/check-url-latency
  '("https://www.google.com"
    "https://www.baidu.com"
    "https://www.bing.com"
    "https://www.bilibili.com"
    "https://www.youtube.com"
    "https://www.github.com"))

(defun lou/enable-http-proxy ()
  "启用 HTTP 和 HTTPS 代理。"
  (interactive)
  (let ((http-proxy-full-url  (format "%s:%s" lou/http-proxy-url lou/http-proxy-port))
        (https-proxy-full-url (format "%s:%s" lou/https-proxy-url lou/https-proxy-port)))
    (setq url-proxy-services
          `(("http"  . ,http-proxy-full-url)
            ("https" . ,https-proxy-full-url)))
        (message "HTTP/HTTPS 代理已启用 (HTTP->%s, HTTPS->%s)" http-proxy-full-url https-proxy-full-url)))

(defun lou/enable-socks-proxy ()
  "启用 SOCKS 代理。"
  (interactive)
  (setq socks-server (list "Default server" 
                           lou/socks-proxy-url
                           (string-to-number lou/socks-proxy-port) 
                           (string-to-number lou/socks-proxy-version)))
  (message "SOCKS 代理已启用: %s:%s" lou/socks-proxy-url lou/socks-proxy-port))

(defun lou/enable-all-proxies ()
  "启用所有代理。"
  (interactive)
  (lou/enable-http-proxy)
  (lou/enable-socks-proxy))

(defun lou/disable-all-proxies ()
  "禁用所有代理。"
  (interactive)
  (setq url-proxy-services nil)
  (setq socks-server nil)
  (message "所有代理已禁用"))

(defun lou/proxy-show-status ()
  "显示当前详细的代理状态。"
  (interactive)
  (message "--- Emacs 代理状态 ---")
  (if url-proxy-services
      (progn
        (message "[HTTS] 已启用:")
                (let ((http-proxy-full-url (assoc "http" url-proxy-services))
              (https-proxy-full-url (assoc "https" url-proxy-services)))
          (when http-proxy-full-url (message "HTTP: %s" (cdr http-proxy-full-url)))
          (when https-proxy-full-url (message "HTTPS: %s" (cdr https-proxy-full-url)))))
    (message "[HTTP/HTTPS] 未启用 (直连)"))
  (if socks-server
      (message "[SOCKS] 已启用: %s:%s" (nth 1 socks-server) (nth 2 socks-server))
    (message "[SOCKS] 未启用 (直连)"))
  (message "----------------------"))

(defun lou/switch-proxy-dwim ()
  (interactive)
  (cond
   ((and (bound-and-true-p socks-server)       
         (bound-and-true-p url-proxy-services))
    (lou/disable-all-proxies))
   ((and (bound-and-true-p url-proxy-services)
         (not (bound-and-true-p socks-server)))
    (lou/enable-socks-proxy))
   (t
    (lou/enable-http-proxy))))

(defun lou/check-proxy-info ()
  (interactive)
  (message "正在连接 %s 获取信息..." lou/check-proxy-ip-url)
  (let ((response-buffer (url-retrieve-synchronously lou/check-proxy-ip-url)))
    (if (not response-buffer)
        (message "请求失败：无法连接到 %s ，请检查网络或代理设置。" lou/check-proxy-ip-url)
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (if (search-forward "\n\n" nil t)
              (condition-case err
                  (let* ((json-object-type 'hash-table)
                         (json-data (json-read))
                         (ip       (gethash "ip" json-data))
                         (city     (gethash "city" json-data))
                         (region   (gethash "region" json-data))
                         (country  (gethash "country" json-data))
                         (timezone (gethash "timezone" json-data)))
                    (message "网络身份报告:\n----------------\nIP 地址 : %s\n位置    : %s, %s, %s\n时区    : %s"
                             ip city region country timezone))
                (json-error (message "JSON 解析错误: %s" err)))
            (message "数据格式错误：无法找到 JSON 内容起始点"))
          (kill-buffer response-buffer)))))

(defun lou/check-proxy-latency ()
  (interactive)
  (message "正在检测网站延迟，请稍候... (%s)" (current-time-string))
  (message "----------------------------------")
  (let ((results ()))
    (dolist (url lou/check-url-latency)
      (let* ((start-time (float-time))
             (response-buffer nil)    
             (latency-ms nil)         
             (status "连接失败或超时"))
        (condition-case err
            (setq response-buffer (url-retrieve-synchronously url))
          (error
           (setq status (format "错误: %s" (cadr err)))))
        (when response-buffer
          (kill-buffer response-buffer)
          (setq latency-ms (* 1000 (- (float-time) start-time)))
          (setq status (format "%.2f ms" latency-ms)))
        (push (format "  %-25s: %s" url status) results)))
    (setq results (nreverse results))
    (message "网站延迟检测报告完成:\n%s\n----------------------------------"
             (string-join results "\n"))))

;; File Encoding Conversion
;; 遇到更难顶的编码就用 unicad 试试吧
;; 将当前文件以选择的编码（务必要正确）重新打开，并保存为 UTF-8 编码
(defun lou/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;; 将当前文件以 GBK 编码重新打开，并保存为 UTF-8 编码
(defun lou/save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (lou/save-buffer-as-utf8 'gbk))

;; Fonts
(defun lou/switch-to-proportional-font ()
  (interactive)
  (variable-pitch-mode 1)
  (text-scale-increase 1))


(provide 'init-lou)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; init-lou.el ends here
