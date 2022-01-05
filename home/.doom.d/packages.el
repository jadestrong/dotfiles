;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! js-doc)
(package! react-snippets)
(package! evil-matchit)
(package! pinyinlib)
(package! leetcode :recipe (:host github :repo "jadestrong/leetcode.el"))
(package! insert-translated-name :recipe (:host github :repo "manateelazycat/insert-translated-name"))
(when (featurep! :completion company)
  ;; (package! company-tabnine :recipe (:host github :repo "TommyX12/company-tabnine"))
  (package! citre :recipe (:host github :repo "universal-ctags/citre" :files ("*.el")))
  (package! company-quickhelp :recipe (:host github :repo "company-mode/company-quickhelp")))
(package! eslintd-fix)
(package! prescient :recipe (:host github :repo "raxod502/prescient.el" :files ("*.el")))
(package! doom-todo-ivy :recipe (:host github :repo "jsmestad/doom-todo-ivy"))
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
;; (package! tsc :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("core/*.el")))
;; (package! tree-sitter :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("lisp/*.el")))
;; (package! tree-sitter-langs :recipe (:host github :repo "ubolonton/emacs-tree-sitter" :files ("langs/*.el" "langs/queries")))
