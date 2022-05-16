;;; editor/god-mode/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :config
  (after! which-key
    (which-key-enable-god-mode-support)))

(use-package! evil-god-state
  :after (god-mode evil)
  :config
  (evil-define-key 'normal global-map (kbd "SPC l") 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
