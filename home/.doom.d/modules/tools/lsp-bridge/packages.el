;; -*- no-byte-compile: t; -*-
;;; tools/lsp-bridge/packages.el
;; (package! lsp-bridge
;;   :recipe (
;;            :host github
;;            :repo "manateelazycat/lsp-bridge"
;;            :files ("*.el" "*.py" "core/*.py" "langserver/*.json" "multiserver/*.json" "acm/*")))
;; (package! lsp-bridge-diagnostics :recipe (:host github :repo "jadestrong/lsp-bridge-diagnostics"))
;; (package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")))
;; (package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc"))
;; (package! orderless)


;; (package! lsp-bridge
;;   :recipe (:host github
;;            :repo "manateelazycat/lsp-bridge"
;; 	   :files ("*")))


;; (package! lsp-mode :disable t :ignore t)
(package! company :disable t :ignore t)
