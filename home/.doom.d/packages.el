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
(package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(when (featurep! :completion company)
  (package! company-lsp :recipe (:host github :repo "jadestrong/company-lsp"))
  (package! company-tabnine :recipe (:host github :repo "TommyX12/company-tabnine")))
(package! eslintd-fix)
;; (package! leetcode-emacs :recipe
;;   (:host github :repo "ginqi7/leetcode-emacs"))
