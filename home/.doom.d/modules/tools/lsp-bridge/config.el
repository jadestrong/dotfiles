;;; tools/lsp-bridge/config.el -*- lexical-binding: t; -*-

(use-package! corfu
  :init
  (require 'corfu-info)
  :config
  (setq corfu-auto nil)
  (setq corfu-auto-prefix 0)
  (setq corfu-preview-current nil)
  (setq corfu-preselect-first t)
  (setq corfu-on-exact-match 'quit)
  (global-corfu-mode))

(use-package! corfu-doc)

(use-package! lsp-bridge
  :init
  (require 'lsp-bridge-orderless)
  (require 'lsp-bridge-icon)
  :config
  (setq lsp-bridge-enable-log nil)
  ;; (setq lsp-bridge-enable-auto-import t)
  (setq lsp-bridge-completion-stop-commands '(corfu-complete corfu-insert undo-tree-undo undo-tree-redo save-buffer evil-normal-state))
  (set-lookup-handlers! 'lsp-bridge-mode
    :definition #'lsp-bridge-find-def
    :references #'lsp-bridge-find-references
    :documentation #'lsp-bridge-lookup-documentation
    :implementations #'lsp-bridge-find-impl)

  (defadvice! ++javascript-init-lsp-or-tide-maybe-h ()
    :override #'+javascript-init-lsp-or-tide-maybe-h
    nil)

  (global-lsp-bridge-mode))

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
                         ))
(setq vls-project-list '(
                         "/Users/jadestrong/Documents/JadeStrong/ugfe-nebula"
                         ))
(defun lsp-bridge--get-lang-server-by-project (project-path file-path)
  "Get lang server by PROJECT-PATH ANF FILE_PATH."
  (cond ((and (member project-path vls-project-list)
              (string-equal (file-name-extension file-path) "vue"))
         (my/get-private-langserver "vls.json"))
        ((or (member project-path volar-project-list)
             (and (not (member project-path vls-project-list)) (lsp-bridge--vue-project-p project-path)))
         (my/get-private-langserver "volar.json"))
        (t nil)))
(setq lsp-bridge-get-lang-server-by-project #'lsp-bridge--get-lang-server-by-project)
