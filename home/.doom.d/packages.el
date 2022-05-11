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
(package! org-modern :recipe (:host github :repo "minad/org-modern"))
(package! dirvish :recipe (:host github :repo "alexluigit/dirvish" :files ("*.el" "extensions/*.el")))

(package! gif-screencast :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(unpin! lsp-mode)
(package! lsp-mode :recipe (:host github :repo "jadestrong/lsp-mode" :branch "master"))
(package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))

(unpin! org-roam)
(package! org-roam-ui)

;; (package! xwwp :recipe (:host github :repo "canatella/xwwp"))
(package! xwidget-webkit-vimium :recipe (:host github :repo "jadestrong/xwidget-webkit-vimium"))

;; (package! lsp-bridge :recipe (:host github :repo "jadestrong/lsp-bridge" :files ("*.el" "*.py" "core/*.py" "langserver/*.json")))
;; (package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")))
(package! lsp-pyright :recipe (:host github :repo "emacs-lsp/lsp-pyright"))
