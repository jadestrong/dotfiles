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
(package! rime :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("rime.el" "rime-predicates.el" "Makefile" "lib.c")))
(package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(package! eslintd-fix)
;; (package! nox :recipe (:host github :repo "manateelazycat/nox"))
;; (package! leetcode-emacs :recipe
;;   (:host github :repo "ginqi7/leetcode-emacs"))
