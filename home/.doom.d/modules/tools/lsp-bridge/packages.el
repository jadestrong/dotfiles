;; -*- no-byte-compile: t; -*-
;;; tools/lsp-bridge/packages.el
(package! lsp-bridge
  :recipe (
           :host github
           :repo "manateelazycat/lsp-bridge"
           :files ("*.el" "*.py" "core/*.py" "langserver/*.json" "acm/*")))
(package! lsp-bridge-diagnostics :recipe (:host github :repo "jadestrong/lsp-bridge-diagnostics"))
;; (package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")))
;; (package! corfu-doc :recipe (:host github :repo "galeo/corfu-doc"))
;; (package! orderless)
