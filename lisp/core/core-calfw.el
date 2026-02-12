;;; core-calfw.el --- Core Calfw -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package cal-china-x
  :ensure t
  :init
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-force-chinese-week-day t)
  :config
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")
                                       (holiday-solar-term "谷雨" "谷雨")
                                       (holiday-solar-term "冬至" "冬至")
                                       ))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

(use-package calfw
  :ensure t
  :bind ((:map global-map
               ("<f9>" . calfw-open-calendar-buffer))
         (:map calfw-calendar-mode-map
               ("-" . calfw-navi-previous-month-command)
               ("=" . calfw-navi-next-month-command)
               ("+" . calfw-navi-next-month-command)
               ("A" . calfw-navi-goto-first-date-command)
               ("E" . calfw-navi-goto-last-date-command)
               ("a" . calfw-navi-goto-week-begin-command)
               ("e" . calfw-navi-goto-week-end-command)
               ("<" . calfw-navi-prev-view)
               (">" . calfw-navi-next-view)
               (","  . calfw-show-details-command)
               ("d" . calfw-show-details-command)))

  :config
  ;; Month
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月"
         "七月" "八月" "九月" "十月" "十一月" "十二月"])
  ;; Week days
  (setq calendar-day-name-array
        ["周日" "周一" "周二" "周三" "周四" "周五" "周六"])
  ;; First day of the week
  (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

  (setq calfw-render-line-breaker 'calfw-render-line-breaker-wordwrap) ;; 待实践

  (with-eval-after-load 'meow
    (add-hook 'calfw-calendar-mode-hook  #'meow-insert-mode)))

(use-package life-calendar
  :ensure t
  :config
  ;; Your birthday (set automatically on first use, or set manually).
  ;; (setq life-calendar-birthday "1990-05-15")

  ;; Number of years to display (default: 90).
  (setq life-calendar-years 100)

  ;; When to count a new week (default: 'birthday).
  ;; 'birthday = day of week you were born (each week starts on that day)
  ;; Or use a day symbol: 'sunday, 'monday, 'tuesday, etc.
  (setq life-calendar-week-start-day 'sunday)

  ;; Number of columns to display (default: nil).
  ;; nil = auto-fit as many columns as the window allows
  ;; Or set to a specific number for fixed columns.
  (setq life-calendar-columns nil)   ; auto-fit to window
  ;; (setq life-calendar-columns 2)     ; always show 2 columns

  ;; Characters for different week states.
  ;; (setq life-calendar-past-char "●")
  ;; (setq life-calendar-current-char "●")
  ;; (setq life-calendar-future-char "○")
  ;; (setq life-calendar-chapter-char "★")
  
  )

(provide 'core-calfw)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; no-byte-compile: t
;; End:

;;; core-calfw.el ends here
