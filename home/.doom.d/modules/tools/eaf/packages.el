;;; tools/eaf/pacakges.el -*- lexical-binding: t; -*-
(package! eaf :recipe (:host github
                       :repo "emacs-eaf/emacs-application-framework"
                       :files ("eaf.el" "*.py" "core" "extension" "*.json")))
(package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-demo" :files ("*")))
(package! eaf-browser :recipe (:host github
                               :repo "emacs-eaf/eaf-browser"
                               :files ("*")
                               :pre-build ("npm" "install")))
(package! eaf-file-manager :recipe (:host github
                                    :repo "emacs-eaf/eaf-file-manager"
                                    :files ("*")
                                    :pre-build ("npm" "install")))
(package! eaf-file-browser :recipe (:host github
                                    :repo "emacs-eaf/eaf-file-browser"
                                    :files ("*.el" "*.py" "*.json")))

(package! eaf-airshare :recipe (:host github
                                    :repo "emacs-eaf/eaf-airshare"
                                    :files ("*.el" "*.py" "*.json")))

(package! eaf-pdf-viewer :recipe (:host github
                                    :repo "emacs-eaf/eaf-pdf-viewer"
                                    :files ("*.el" "*.py" "*.json")))

(package! eaf-image-viewer :recipe (:host github
                                    :repo "emacs-eaf/eaf-image-viewer"
                                    :files ("*")
                                    :pre-build ("npm" "install")))

(package! eaf-netease-cloud-music :recipe (:host github
                                    :repo "emacs-eaf/eaf-netease-cloud-music"
                                    :files ("*")
                                    :pre-build ("npm" "install")))

(package! eaf-terminal :recipe (:host github
                                    :repo "emacs-eaf/eaf-terminal"
                                    :files ("*")
                                    :pre-build ("npm" "install")))


;; (package! eaf-evil :recipe (:host github :repo "emacs-eaf/emacs-application-framework :"))
;; (package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-file-manager"))
;; (package! eaf-demo :recipe (:host github :repo "emacs-eaf/eaf-file-manager"))
