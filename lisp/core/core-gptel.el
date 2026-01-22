;;; core-gptel.el --- Core Gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :ensure t
  :bind
  (:map global-map
        ("C-c g" . gptel)        
        ("C-c RET" . gptel-send))
  ;;       ("C-c ," . gptel-menu)
  
  :config
  (require 'gptel-integrations)
  
  (setq gptel-default-mode 'org-mode)
  (setq gptel-stream t)
  (setq gptel-use-curl t)
  (setq gptel-track-media t)
  
  ;; 待研究并配置
  ;; (setq gptel-max-tokens )
  
  ;; 是否跟踪并记录 GPT 的回复内容
  (setq gptel-track-response t)
  ;; 设置 GPT 回答的随机性（1.0 表示平衡、自然的回答）
  (setq gptel-temperature 1.0)
  ;; 是否允许 GPT 使用工具（如函数调用等）
  (setq gptel-use-tools t)
  ;; 是否在 Emacs 窗口顶部显示 gptel 的状态信息
  (setq gptel-use-header-line t)
  ;; 是否在回复中包含推理过程（思考步骤）
  (setq gptel-include-reasoning t)
  ;; 是否启用专家级命令（新手一般关闭）
  (setq gptel-expert-commands nil)
  ;; 设置日志级别，nil 表示不记录日志
  (setq gptel-log-level nil)
  
  ;; 不够好看，待研究
  ;; https://github.com/karthink/gptel?tab=readme-ov-file#i-want-to-change-the-formatting-of-the-prompt-and-llm-response
  ;; (gptel-highlight-mode +1)

  (setq gptel-prompt-prefix-alist '((markdown-mode . "Lou: ")
                                    (org-mode . "Lou: ")
                                    (text-mode . "Lou: ")))
  (setq gptel-response-prefix-alist '((markdown-mode . "Navi: ")
                                      (org-mode . "Navi: ")
                                      (text-mode . "Navi: ")))

  (setq gptel-model 'Claude-Opus-4.5
        gptel-backend
        (gptel-make-openai "zaiwen"        ;Any name you want
          :host "back.zaiwenai.com"
          :endpoint "/api/v1/ai/chat/completions"
          :stream t
          :key 'gptel-api-key
          :models '(gpt-5.2-chat-latest Gemini-3.0-Pro Claude-Opus-4.5 Gemini-3.0-Flash Grok-4.1-Fast-Non-Reasoning)))
  
  ;; 预设
  (gptel-make-preset 'Explain-code
    :description "Explain what this code does to a novice programmer."
    :system "Explain what this code does to a novice programmer."
    :backend "zaiwen"
    :model 'gpt-5.2-chat-latest
    ;; :tools '("read_buffer" "spell_check" "grammar_check")
    :use-context 'system                                ;sets gptel-use-context
    ;; :context '("./.grammar_rules.md" "./jargonfile.md") ;sets gptel-context
    :temperature 0.2)                                   ;sets gptel-temperature

  )

;; 添加相关工具给 gptel 使用
;; https://github.com/karthink/gptel?tab=readme-ov-file#llm-tool-collections

;; 如何使用待研究
(use-package gptel-agent
  :ensure (gptel-agent
           :host github
           :repo "karthink/gptel-agent"
           :files ("*" (:exclude ".git")))
  :after gptel
  :config
  ;; Read files from agents directories
  (gptel-agent-update))


;; 完成编程语言配置后再来配置这个
;; (use-package mcp
;;   :straight t
;;   :after gptel
;;   :custom (mcp-hub-servers
;;            `(("filesystem" . (:command "npx"
;;                               :args ("-y" "@modelcontextprotocol/server-filesystem")
;;                               :roots ("/home/lizqwer/MyProject/")))
;;              ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
;;              ("qdrant" . (:url "http://localhost:8000/sse"))
;;              ("graphlit" . (
;;                             :command "npx"
;;                             :args ("-y" "graphlit-mcp-server")
;;                             :env (
;;                                   :GRAPHLIT_ORGANIZATION_ID "your-organization-id"
;;                                   :GRAPHLIT_ENVIRONMENT_ID "your-environment-id"
;;                                   :GRAPHLIT_JWT_SECRET "your-jwt-secret")))))
;;   :config (require 'mcp-hub)
;;   :hook (after-init . mcp-hub-start-all-server))

;; (use-package gptel-mcp
;;   :straight (:type git :host github :repo "lizqwerscott/gptel-mcp.el")
;;   :bind (:map gptel-mode-map
;;               ("C-c m" . gptel-mcp-dispatch)))

(provide 'core-gptel)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-gptel.el ends here
