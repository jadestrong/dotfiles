;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! evil-matchit)
(package! leetcode :recipe (:host github :repo "jadestrong/leetcode.el"))

(when (modulep! :tools lsp)
  (package! project :built-in t))
;; (unpin! doom-modeline)
;; (unpin! rustic)
;; (unpin! org-roam)
;; (package! org-roam-ui)
(unpin! editorconfig)
;; (package! emacsql-sqlite-builtin)
;; (unpin! (:completion company) (:completion corfu) (:editor evil))
;; (package! xwwp :recipe (:host github :repo "canatella/xwwp"))
(package! jsdoc :recipe (:host github :repo "isamert/jsdoc.el"))
(package! vimrc-mode)
(package! tide :disable t)
(package! olivetti :recipe (:host github :repo "rnkn/olivetti"))
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))
(package! gptel)

;; (package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el")))
(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))
(package! ivy-xref :recipe (:host github :repo "jdormit/ivy-xref"))

(package! magit-gitcommit :recipe (:host github :repo "douo/magit-gptcommit" :branch "gptel" :files ("*.el")))

;; (package! lsp-copilot :recipe (:host github :repo "jadestrong/lsp-copilot"
;;                 :files ("lsp-copilot.el" "lsp-copilot")
;;                 :pre-build (("cargo" "build" "--release") ("cp" "./target/release/lsp-copilot" "./"))))

;; (package! tabnine)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
