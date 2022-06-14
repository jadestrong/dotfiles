;;; tools/lsp-bridge/config.el -*- lexical-binding: t; -*-

;; (use-package! corfu
;;   :init
;;   (require 'corfu-info)
;;   :config
;;   (setq corfu-auto nil)
;;   (setq corfu-auto-prefix 0)
;;   (setq corfu-preview-current nil)
;;   (setq corfu-preselect-first t)
;;   (setq corfu-on-exact-match 'quit)
;;   (global-corfu-mode))

;; (use-package! corfu-doc)

(use-package! lsp-bridge
  :hook (doom-first-buffer . global-lsp-bridge-mode)
  :init
  (require 'acm) ;; Fix acm-silent is a void function error
  :config
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-enable-diagnostics nil)
  (setq acm-enable-dabbrev nil)
  ;; (setq lsp-bridge-enable-auto-import t)
  ;; (setq lsp-bridge-completion-stop-commands '(corfu-complete corfu-insert undo-tree-undo undo-tree-redo save-buffer evil-normal-state))
  (set-lookup-handlers! 'lsp-bridge-mode
    :definition #'lsp-bridge-find-def
    :references #'lsp-bridge-find-references
    :documentation #'lsp-bridge-lookup-documentation
    :implementations #'lsp-bridge-find-impl)
  ;; Above setter will override elisp's definition handler
  (set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode helpful-mode)
    :definition    #'+emacs-lisp-lookup-definition
    :documentation #'+emacs-lisp-lookup-documentation)

  (defadvice! ++javascript-init-lsp-or-tide-maybe-h ()
    :override #'+javascript-init-lsp-or-tide-maybe-h
    nil)

  ;; fix Error running timer ‘acm-idle-completion’: (void-variable with)
  (defadvice! +acm-backend-dabbrev-get-words (word)
    :override #'acm-backend-dabbrev-get-words
    (require 'dabbrev)
    (acm-silent
      (let ((dabbrev-check-other-buffers nil)
            (dabbrev-check-all-buffers nil))
        (dabbrev--reset-global-variables))
      (let ((min-len (+ acm-backend-dabbrev-min-length (length word))))
        (cl-loop for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
                 if (>= (length w) min-len) collect w))))

  (setq lsp-bridge-lang-server-mode-list
        '(
         ;; ((c-mode c++-mode) . "clangd")
         ;; (java-mode . "jdtls")
         (python-mode . "pyright")
         ;; (ruby-mode . "solargraph")
         ;; ((rust-mode rustic-mode) . "rust-analyzer")
         ;; (elixir-mode . "elixirLS")
         ;; (go-mode . "gopls")
         ;; (haskell-mode . "hls")
         ;; (lua-mode . "sumneko")
         ;; (dart-mode . "dart-analysis-server")
         ;; (scala-mode . "metals")
         ((js2-mode js-mode rjsx-mode) . "javascript")
         (typescript-tsx-mode . "typescriptreact")
         ((typescript-mode) . "typescript")
         ;; (tuareg-mode . "ocamllsp")
         ;; (erlang-mode . "erlang-ls")
         ;; ((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) . "texlab")
         ;; ((clojure-mode clojurec-mode clojurescript-mode clojurex-mode) . "clojure-lsp")
         ;; ((sh-mode) . "bash-language-server")
         ;; ((css-mode) . "vscode-css-language-server")
         ;; (elm-mode . "elm-language-server")
         ))
  (when (featurep! :completion company)
    (setq company-global-modes
          '(not erc-mode
                circe-mode
                message-mode
                help-mode
                gud-mode
                vterm-mode
                emacs-lisp-mode)))

  ;; (global-lsp-bridge-mode)
  )

(use-package! lsp-bridge-diagnostics
  :after (lsp-bridge)
  :config
  (setq lsp-bridge-diagnostics-provider :flycheck))

(defun lsp-bridge--vue-project-p (project-path)
  (if-let ((package-json (f-join project-path "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json))
           (dependencies (alist-get 'dependencies config)))
      (alist-get 'vue dependencies)
    nil))

(defun my/get-private-langserver (file-name)
  (f-join doom-private-dir "langserver" file-name))

(setq volar-project-list '(
                         "/media/psf/Home/Documents/JadeStrong/olympic-game"
                         "/Users/jadestrong/Documents/JadeStrong/ugfe-activity"
                         "/Users/jadestrong/Documents/JadeStrong/olympic-game"
                         ))
(setq vls-project-list '(
                         "/Users/jadestrong/Documents/JadeStrong/ugfe-nebula"
                         "/Users/jadestrong/Documents/JadeStrong/ugfe-orchard"
                         ))
(defun lsp-bridge--get-lang-server-by-project (project-path file-path)
  "Get lang server by PROJECT-PATH ANF FILE_PATH."
  (cond ((and (member project-path vls-project-list)
              (string-equal (file-name-extension file-path) "vue"))
         (my/get-private-langserver "vls.json"))
        ((or (member project-path volar-project-list)
             (and (not (member project-path vls-project-list)) (lsp-bridge--vue-project-p project-path)))
         (my/get-private-langserver "volar.json"))
        ((string-equal (file-name-extension file-path) "rs")
         (my/get-private-langserver "rust-analyzer.json"))
        (t nil)))
(setq lsp-bridge-get-lang-server-by-project #'lsp-bridge--get-lang-server-by-project)
