;;; editor/god-mode/config.el -*- lexical-binding: t; -*-

(use-package! god-mode
  :config
  (after! which-key
    (which-key-enable-god-mode-support)))

(use-package! evil-god-state
  :after (god-mode evil)
  :config
  (evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
  (evil-define-key 'god global-map [escape] 'evil-god-state-bail)
  ;; god-mode support emacs-rime
  (defadvice! +rime-predicate-evil-mode-p ()
    :override #'rime-predicate-evil-mode-p
    (and (fboundp 'evil-mode)
       (or (evil-normal-state-p)
           (evil-visual-state-p)
           (evil-motion-state-p)
           (evil-god-state-p)
           (evil-operator-state-p)))))
