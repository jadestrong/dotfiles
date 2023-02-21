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
;; (when (modulep! :completion company)
;;   (package! citre :recipe (:host github :repo "universal-ctags/citre" :files ("*.el"))))
(package! eslintd-fix)
(package! org-modern :recipe (:host github :repo "minad/org-modern"))
(package! dirvish :recipe (:host github :repo "alexluigit/dirvish" :files ("*.el" "extensions/*.el")))

;; (package! gif-screencast :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(when (modulep! :tools lsp)
  (unpin! lsp-mode)
  (package! lsp-mode :recipe (:host github :repo "jadestrong/lsp-mode" :branch "master"))
  (package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
  ;; (unpin! eglot)
  ;; (package! eglot :built-in t)
  (package! project :built-in t)
  ;; (package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
  )

(unpin! doom-modeline)
(unpin! rustic)
(unpin! magit)
(unpin! org-roam)
(package! org-roam-ui)
(unpin! editorconfig)
(package! emacsql-sqlite-builtin)

;; (package! xwwp :recipe (:host github :repo "canatella/xwwp"))
(package! xwidget-webkit-vimium :recipe (:host github :repo "jadestrong/xwidget-webkit-vimium"))

;; (package! lspce :recipe (:local-repo "extensions/lspce"
;;                          :build (:not compile)
;;                          :pre-build ((shell-command "cargo build --release && ln -s -f target/release/liblspce_module.dylib lspce-module.dylib"))))
;; (package! posframe-plus :recipe (:host github :repo "zbelial/posframe-plus"))

;; (package! lsp-rocks :recipe (:local-repo "extensions/lsp-rocks"
;;                              :files ("*.el" "lib")
;;                              ;; :build (:not compile)
;;                              :pre-build ((shell-command "pnpm run build"))))

;; (package! emacs-async)
;; (package! greeting :recipe (:local-repo "extensions/greeting"))

;; (package! jsdoc :recipe (:host github :repo "isamert/jsdoc.el"))

;; (package! tsi :recipe (:host github :repo "orzechowskid/tsi.el"))
;; (package! tsx-mode :recipe (:host github :repo "orzechowskid/tsx-mode.el"))

(package! apheleia)
;; (package! plantuml :recipe (:host github :repo "ginqi7/plantuml-emacs"))

(package! epc)
(package! vimrc-mode)

;; (package! coc :recipe (:local-repo "lisp/coc.emacs"
;;                        :files ("*.el" "index.js" "dist/*")
;;                        :pre-build ((shell-command "pnpm install && pnpm build && pnpm run bundle"))))
(package! tide :disable t)

; (package! websocket-bridge :recipe (:host github :repo "ginqi7/websocket-bridge"))
; (package! dictionary-overlay :recipe (:host github :repo "ginqi7/dictionary-overlay" :files ("*")))

(package! olivetti :recipe (:host github :repo "rnkn/olivetti"))

(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))
