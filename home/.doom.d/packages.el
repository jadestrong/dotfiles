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
(package! vimrc-mode)
(package! tide :disable t)
(package! company-sourcekit :disable t)
(package! olivetti :recipe (:host github :repo "rnkn/olivetti"))
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))

;; (package! ivy-xref :recipe (:host github :repo "jdormit/ivy-xref"))

;; (package! tabnine)
;; (package! copilot
;;   :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(package! claude-code :recipe (:host github :repo "stevemolitor/claude-code.el"
                               :files ("*.el" (:exclude "images/*"))))

(when (modulep! :tools magit +forge)
  (unpin! code-review)
  (package! code-review
    :recipe (:host github
             :repo "jadestrong/code-review"
             :files ("graphql" "code-review*.el"))
    :pin "498d5a9d3fae39e9111db37d961db676c501059c"
    ;; ...code-review depends on forge
    :disable (version< emacs-version "29.1")))

(package! expreg)
