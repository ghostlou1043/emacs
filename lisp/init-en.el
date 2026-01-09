;;; init-en.el --- Init EN -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package gt
  :ensure t
  :bind
  (:map global-map
        ("C-?" . gt-translate) ;; 默认翻译绑定
        ("C-c ?" . gt-switch-translator))
  
  ;;       ("C-t k" . gt-record-words-as-known)  等 paw 配置完成再看是否需要它
  ;;       ("C-t n" . gt-record-words-as-unkown)
  
  ;;       ("C-t m" . kaladin/translate-function-name-to-camel-case) ;; 需要先执行一次 gt-do-translate 才能正常使用以下函数，待修复
  ;;       ("C-t a" . kaladin/translate-function-name-to-snake-case)
  ;;       ("C-t -" . kaladin/translate-function-name-to-kebab-case)
  ;;       ("C-t ;" . kaladin/translate-comment-to-en)
  ;;       ("C-t '" . kaladin/translate-comment-to-cn)
  ;;       ("C-t h" . kaladin/translate-line-to-cn)
  ;;       )
  
  :config
  (setq gt-cache-p t)
  (setq gt-langs '(en zh))        ; 默认的翻译语言，至少要指定两个语言
  (setq gt-taker-text 'word)      ; 默认情况下，初始文本是光标下的单词。如果有文本选中，优先使用选中文本
  (setq gt-taker-pick 'paragraph) ; 默认情况下，会按照段落标准分割初始文本。如果不想使用多段翻译，将其设置为 nil
  (setq gt-taker-prompt nil)      ; 默认情况下，没有 prompt 步骤。如果需要，将其设置为 t 或 'buffer
  (setq gt-default-translator nil)

  ;; (gt-translator :taker (gt-taker :langs '(en zh ru))) ; 在中、英、俄之间进行翻译
  ;; (setq gt-polyglot-p t) ; 如果将此变量设置为 t，那么将进行多语言翻译，即一次翻译成多语言并聚合输出

  (when (executable-find "curl")
    (setq gt-http-backend (pdd-curl-backend)))

  (setq gt-chatgpt-host "https://back.zaiwenai.com")
  (setq gt-chatgpt-path "/api/v1/ai/chat/completions")
  (setq gt-chatgpt-model "Grok-4.1-Fast-Non-Reasoning")
  (setq gt-chatgpt-temperature 1)

  ;; text 获取初始文本	范围可以是当前界面 (buffer) 、当前段落 (paragraph) 、当前句子 (sentence) 、当前单词 (word)
  ;; pick 对 text 获取到的文本进行分段 (paragraph)、分句 (sentence) 、分词 (word)

  (setq gt-preset-translators
        ;; 将中文的句或词翻译为英文，将英文单词翻译为中文
        `((通用 . ,(gt-translator
                    :taker (gt-taker :langs '(en zh) :text 'word :pick 'paragraph :prompt t)
                    :engines (list (gt-stardict-engine
                                    :if '(and word  src:en)
                                    :dir (expand-file-name "~/.stardict/dic")
                                    :dict "朗道英汉字典5.0"
                                    :exact t)
                                   (gt-stardict-engine
                                    :if '(and word  src:zh)
                                    :dir (expand-file-name "~/.stardict/dic")
                                    :dict "朗道汉英字典5.0")
                                   (gt-google-engine :if 'not-word)
                                   (gt-youdao-dict-engine :if 'word)
                                   (gt-youdao-suggest-engine :if 'word))
                    :render (list (gt-buffer-render :if 'word) ; 如果翻译的是单词，那么通过 buffer 方式渲染
                                  (gt-posframe-pop-render :then (gt-kill-ring-render)))))

          ;; 通用失效时的备用
          (默认备用 . ,(gt-translator
                        :taker (gt-taker :langs '(en zh) :text 'word :pick 'paragraph :prompt t)
                        :engines (list (gt-stardict-engine
                                        :if '(and word  src:en)
                                        :dir (expand-file-name "~/.stardict/dic")
                                        :dict "朗道英汉字典5.0"
                                        :exact t)
                                       (gt-stardict-engine
                                        :if '(and word  src:zh)
                                        :dir (expand-file-name "~/.stardict/dic")
                                        :dict "朗道汉英字典5.0")
                                       (gt-bing-engine :if 'word)
                                       (gt-google-engine :if 'not-word))
                        :render (list (gt-buffer-render :if 'word) ; 如果翻译的是单词，那么通过 buffer 方式渲染
                                                  (gt-posframe-pop-render :then (gt-kill-ring-render)))))

          ;; 中英句子互译
          (谷歌中英当前句子互译 . ,(gt-translator
                                    :taker (gt-taker :langs '(en zh) :text 'sentence :pick 'paragraph :prompt t)
                                    :engines (list (gt-stardict-engine
                                                    :if '(and word  src:en)
                                                    :dir (expand-file-name "~/.stardict/dic")
                                                    :dict "朗道英汉字典5.0"
                                                    :exact t)
                                                   (gt-stardict-engine
                                                    :if '(and word  src:zh)
                                                    :dir (expand-file-name "~/.stardict/dic")
                                                    :dict "朗道汉英字典5.0")
                                                   (gt-youdao-dict-engine :if 'word)
                                                   (gt-youdao-suggest-engine :if 'word)
                                                   (gt-google-engine :if 'not-word))
                                    ;; 以系统消息方式展示，并保存进 kill-ring
                                    :render (list (gt-buffer-render :if 'word) ; 如果翻译的是单词，那么通过 buffer 方式渲染
                                                  (gt-posframe-pop-render :frame-params
                                                                          (list :border-width 1
                                                                                :border-color "red")
                                                                          :then (gt-kill-ring-render)))))


          (AI中英当前句子互译 . ,(gt-translator
                                      :taker (gt-taker :langs '(en zh) :text 'sentence :pick 'paragraph :prompt t)
                                      :engines (list (gt-stardict-engine
                                                      :if '(and word  src:en)
                                                      :dir (expand-file-name "~/.stardict/dic")
                                                      :dict "朗道英汉字典5.0"
                                                      :exact t)
                                                     (gt-stardict-engine
                                                      :if '(and word  src:zh)
                                                      :dir (expand-file-name "~/.stardict/dic")
                                                      :dict "朗道汉英字典5.0")
                                                     (gt-youdao-dict-engine :if 'word)
                                                     (gt-youdao-suggest-engine :if 'word)
                                                     (gt-chatgpt-engine :if 'not-word))
                                      :render (list (gt-buffer-render :if 'word) ; 如果翻译的是单词，那么通过 buffer 方式渲染
                                              (gt-posframe-pop-render :then (gt-kill-ring-render)))))

          ;; 中英段落分句互译
          (谷歌中英段落分句互译 . ,(gt-translator
                                    :taker (gt-taker :langs '(en zh) :text 'paragraph :pick 'sentence :prompt 'buffer)
                                    :engines (gt-google-engine)
                                    :render  (gt-buffer-render)))

          (AI中英段落分句互译 . ,(gt-translator
                                      :taker (gt-taker :langs '(en zh) :text 'paragraph :pick 'sentence :prompt 'buffer)
                                      :engines (gt-chatgpt-engine)
                                      :render  (gt-buffer-render)))

          ;; 中英当前段落互译
          (谷歌中英当前段落互译 . ,(gt-translator
                                    :taker   (gt-taker :langs '(en zh) :text 'paragraph :pick 'paragraph :prompt 'buffer)
                                    :engines (gt-google-engine)
                                    :render  (gt-buffer-render)))
          (AI中英当前段落互译 . ,(gt-translator
                                      :taker   (gt-taker :langs '(en zh) :text 'paragraph :pick 'paragraph :prompt 'buffer)
                                      :engines (gt-chatgpt-engine)
                                      :render  (gt-buffer-render)))
          ;; 中英分段互译
          (谷歌中英分段互译 . ,(gt-translator
                                :taker   (gt-taker :langs '(en zh) :text 'buffer :pick 'paragraph :prompt 'buffer)
                                :engines (gt-google-engine)
                                :render  (gt-buffer-render)))
          (AI中英分段互译 . ,(gt-translator
                                  :taker   (gt-taker :langs '(en zh) :text 'buffer :pick 'paragraph :prompt 'buffer)
                                  :engines (gt-chatgpt-engine)
                                  :render  (gt-buffer-render)))

          ;; 使用 Google 翻译 buffer 中所有长度大于 6 的单词，将鼠标放到被翻译的单词上后，翻译结果将以 popup 方式显示
          (学习模式 . ,(gt-translator
                        :taker (gt-taker :langs '(en zh) :text 'buffer
                                         :pick 'word :pick-pred (lambda (w) (length> w 6)))
                        :engines (gt-google-engine)
                        :render (gt-overlay-render :type 'help-echo)))
          (QRCode-gen . ,(gt-text-utility
                          :taker (gt-taker :langs '(qrcode) :text 'selection)
                          :render (gt-buffer-render))))))


;; (use-package go-translate
;;   :ensure t
  ;; :init
  ;; (defun kaladin/enter-meow-insert-in-gt-posframe (&rest _)
  ;;   "Enter `meow-insert-mode` in the *GT-Pop-Posframe* buffer after `gt-do-translate`."
  ;;   (let ((posframe-buffer (get-buffer " *GT-Pop-Posframe*")))  ; 确保使用正确的 buffer 名称
  ;;     (when posframe-buffer
  ;;       (with-current-buffer posframe-buffer
  ;;         (meow-insert-mode 1)))))

  ;; (advice-add 'gt-do-translate :after #'kaladin/enter-meow-insert-in-gt-posframe)

  ;; ;; 配置默认的 HTTP 客户端
  ;; (setq gt-default-http-client
  ;;       (lambda (host)
  ;;         (let ((proxy? (string-match-p "google\\|deepl\\|openai" host)))
  ;;           (if (require 'plz nil t)
  ;;               (if proxy?
  ;;                   (gt-plz-http-client :args '("--proxy" "socks5://127.0.0.1:7897"))
  ;;                 (gt-plz-http-client))
  ;;             (if proxy?
  ;;                 (gt-url-http-client :proxies '(("http" . "host:7897") ("https" . "host:7897")))
  ;;               (gt-url-http-client)))))))

(provide 'init-en)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-en.el ends here
