;;; tools/eaf/pacakges.el -*- lexical-binding: t; -*-
(package! eaf :recipe (:host github
                       :repo "emacs-eaf/emacs-application-framework"
                       :files ("eaf.el" "eaf.py" "core" "extension")))
(package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-demo" :files ("*")))
(package! eaf-browser :recipe (:host github
                               :repo "emacs-eaf/eaf-browser"
                               :files ("*")
                               :pre-build ("npm" "install")))
;; (package! eaf-evil :recipe (:host github :repo "emacs-eaf/emacs-application-framework :"))
;; (package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-file-manager"))
;; (package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-file-manager"))
