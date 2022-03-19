;;; tools/eaf/config.el -*- lexical-binding: t; -*-

(use-package! eaf
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (require 'eaf-demo)
  (require 'eaf-browser)
  (require 'eaf-org)
  (require 'eaf-evil)
  (require 'eaf-all-the-icons)
  (require 'eaf-file-manager)
  (require 'eaf-file-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-netease-cloud-music)
  (require 'eaf-airshare)
  (require 'eaf-terminal)
  (setq eaf-browser-enable-adblocker t)
  (setq eaf-evil-leader-key "SPC")
  (defalias 'browser-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll-up "M-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll-down "M-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ;; (define-key key-translation-map (kbd "SPC")
  ;;   (lambda (prompt)
  ;;     (if (derived-mode-p 'eaf-mode)
  ;;         (pcase eaf--buffer-app-name
  ;;           ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
  ;;                          (kbd "SPC")
  ;;                        (kbd eaf-evil-leader-key)))
  ;;           ("pdf-viewer" (kbd eaf-evil-leader-key))
  ;;           ("image-viewer" (kbd eaf-evil-leader-key))
  ;;           (_  (kbd "SPC")))
  ;;       (kbd "SPC"))))
  )
