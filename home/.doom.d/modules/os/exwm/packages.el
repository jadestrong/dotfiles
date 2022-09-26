;; -*- no-byte-compile: t; -*-
;;; os/exwm/packages.el -*-

(package! exwm)
(when (modulep! :editor evil)
  (package! exwm-evil
    :recipe (:host github :repo "LemonBreezes/exwm-evil"))
  (package! exwm-firefox-evil))
(package! exwm-edit)
(package! language-detection)
(package! exwm-mff :recipe (:host github :repo "ieure/exwm-mff") :pin "89206f2e3189f589c27c56bd2b6203e906ee7100")
