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
(unpin! doom-modeline)
(unpin! rustic)
(unpin! org-roam)
(package! org-roam-ui)
(unpin! editorconfig)
;; (package! emacsql-sqlite-builtin)
(unpin! (:tools magit) (:completion company) (:completion corfu) (:checkers syntax) (:editor evil))
;; (package! xwwp :recipe (:host github :repo "canatella/xwwp"))
(package! jsdoc :recipe (:host github :repo "isamert/jsdoc.el"))
(package! vimrc-mode)
(package! tide :disable t)
(package! olivetti :recipe (:host github :repo "rnkn/olivetti"))
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))
(package! gptel)
