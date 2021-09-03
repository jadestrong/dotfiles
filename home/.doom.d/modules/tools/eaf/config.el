;;; tools/eaf/config.el -*- lexical-binding: t; -*-

(use-package! eaf
  :defer t
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (require 'eaf-demo)
  (require 'eaf-browser)
  (require 'eaf-org)
  (require 'eaf-evil)
  (require 'eaf-all-the-icons)
  (setq eaf-browser-enable-adblocker t)
  (define-key key-translation-map (kbd "SPC")
    (lambda (prompt)
      (if (derived-mode-p 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                           (kbd "SPC")
                         (kbd eaf-evil-leader-key)))
            ("pdf-viewer" (kbd eaf-evil-leader-key))
            ("image-viewer" (kbd eaf-evil-leader-key))
            (_  (kbd "SPC")))
        (kbd "SPC"))))
  )
