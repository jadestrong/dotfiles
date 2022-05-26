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
  (add-to-list 'lsp-bridge-enable-popup-predicates
               '((lambda ()
                   (and
                    (< corfu--index 0) ; not select a candidate
                    (or (not (featurep 'evil)) (evil-insert-state-p))))))
  (global-corfu-mode))

(use-package! lsp-bridge
  :init
  (require 'lsp-bridge-orderless)
  (require 'lsp-bridge-icon)
  :config
  (setq lsp-bridge-enable-log nil)
  (setq lsp-bridge-completion-stop-commands '(corfu-complete corfu-insert undo-tree-undo undo-tree-redo save-buffer evil-normal-state))
  (global-lsp-bridge-mode))

(defun lsp-bridge--vue-project-p (project-path)
  (if-let ((package-json (f-join project-path "package.json"))
           (exist (f-file-p package-json))
           (config (json-read-file package-json))
           (dependencies (alist-get 'dependencies config)))
      (alist-get 'vue dependencies)
    nil))

(setq vue-project-list '(
                         "/media/psf/Home/Documents/JadeStrong/olympic-game"
                         "/Users/jadestrong/Documents/JadeStrong/ugfe-activity"
                         ))
(defun lsp-bridge--get-lang-server-by-project (project-path file-path)
  "Get lang server by PROJECT-PATH ANF FILE_PATH."
  (if (or (member project-path vue-project-list)
          (lsp-bridge--vue-project-p project-path))
      "/Users/jadestrong/.doom.d/langserver/volar.json"
    nil))
(setq lsp-bridge-get-lang-server-by-project #'lsp-bridge--get-lang-server-by-project)
