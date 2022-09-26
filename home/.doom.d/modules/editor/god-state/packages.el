;; -*- no-byte-compile: t; -*-
;;; editor/god-mode/packages.el

(package! god-mode)
(when (modulep! :editor evil)
  (package! evil-god-state))
