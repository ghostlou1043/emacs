;;; core-dashboard.el --- Core Dashboard -*- lexical-binding: t -*-
;;; Commentary: Linux,MacOS,NixOS,Windows
;;; Code:

(use-package dashboard
  :ensure t
  :bind(:map dashboard-mode-map
             ("T" . telega)
             ("B" . butterfly)
             ("E" . elfeed)
             ("M" . emms))
  :config
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-projects-backend 'projectile)
  
  (setq dashboard-items
        '((recents   . 5)
          (bookmarks . 5)
          (agenda    . 5)
          (projects  . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (add-hook 'dashboard-mode-hook (lambda () (setq-local mode-line-format nil)))
  (setq dashboard-banner-logo-title "    Hello, 1043.\nWelcome to the Wired.")
  
  ;; (setq dashboard-startup-banner "~/.config/emacs/VisLain.gif")
  ;; (setq dashboard-startup-banner '("~/.config/emacs/VisLain.gif" . ""))
  
  (setq dashboard-show-shortcuts t)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content nil)
  (setq dashboard-navigation-cycle nil)
  (setq dashboard-heading-shorcut-format " [%s]")
  (setq dashboard-item-shortcuts '((recents   . "r")
                                   (bookmarks . "m")
                                   (projects  . "p")
                                   (agenda    . "a")
                                   (registers . "e")))
  
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons nil)
  (setq dashboard-items-default-length 20)
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-banner-title
          dashboard-insert-init-info
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-items
          dashboard-insert-newline
          dashboard-insert-footer))
  (setq dashboard-navigator-buttons
        '(((" " "Telega" "Task for this week"
            (lambda (&rest _) (telega))
            warning "[" "]")
           (" " "Elfeed" "Browse RSS Feeds"
            (lambda (&rest _) (elfeed))
            warning "[" "]")
           (" " "EMMS" "Emacs Multi-Media System"
            (lambda (&rest _) (emms))
            warning "[" "]")
           (" " "Butterfly" "Real world programming!"
            (lambda (&rest _) (butterfly))
            warning "[" "]"))))

  ;; (setq dashboard-item-names '(("Recent Files:"               . "Recently opened files:")
  ;;                            ("Agenda for today:"           . "Today's agenda:")
  ;;                            ("Agenda for the coming week:" . "Agenda:")))

  (dashboard-setup-startup-hook))

(use-package dashboard-ls
  :ensure t
  :config
  (setq dashboard-items '((ls-directories . 5)
                          (ls-files . 5))))


(provide 'core-dashboard)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; core-dashboard.el ends here
