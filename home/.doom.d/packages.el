;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! js-doc)
(package! react-snippets)
(package! evil-matchit)
;; (package! leetcode)
(package! leetcode :recipe (:host github :repo "jadestrong/leetcode.el"))
;; (package! leetcode-emacs :recipe
;;   (:host github :repo "ginqi7/leetcode-emacs"))

;; (package! mmm-mode)
;; (package! mmm-mako :recipe (:host github :repo "emacsmirror/mmm-mako"))
