;; -*- no-byte-compile: t; -*-
;;; tools/lsp-bridge/packages.el
(package! lsp-bridge :recipe (:host github :repo "jadestrong/lsp-bridge" :files ("*.el" "*.py" "core/*.py" "langserver/*.json")))
(package! corfu :recipe (:host github :repo "minad/corfu" :files ("*.el" "extensions/*.el")))
(package! orderless)
