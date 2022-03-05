;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! js-doc)
(package! react-snippets)
(package! evil-matchit)
(package! leetcode :recipe (:host github :repo "jadestrong/leetcode.el"))
;; (package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(when (featurep! :completion company)
  (package! citre :recipe (:host github :repo "universal-ctags/citre" :files ("*.el"))))
(package! eslintd-fix)
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
