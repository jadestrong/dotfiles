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

;; (use-package! lsp-bridge
;; ;;  :hook (doom-first-buffer . enable-lsp-bridge-for-modes)
;;   :init
;;   (require 'acm) ;; Fix acm-silent is a void function error
;;   :config
;;   ;; (setq lsp-bridge-enable-log nil)
;;   (setq lsp-bridge-enable-diagnostics nil)
;;   ;; (setq acm-enable-dabbrev nil)
;;   ;; (setq lsp-bridge-disable-backup nil)
;;   ;; (setq acm-backend-lsp-candidates-max-number 100000)
;;   ;; (setq lsp-bridge-enable-auto-import t)
;;   ;; (setq lsp-bridge-completion-stop-commands '(corfu-complete corfu-insert undo-tree-undo undo-tree-redo save-buffer evil-normal-state))
;;   (set-lookup-handlers! 'lsp-bridge-mode
;;     :definition #'lsp-bridge-find-def
;;     :references #'lsp-bridge-find-references
;;     :documentation #'lsp-bridge-lookup-documentation
;;     :implementations #'lsp-bridge-find-impl)
;;   ;; Above setter will override elisp's definition handler
;;   (set-lookup-handlers! '(emacs-lisp-mode lisp-interaction-mode helpful-mode)
;;     :definition    #'+emacs-lisp-lookup-definition
;;     :documentation #'+emacs-lisp-lookup-documentation)

;;   ;; (defadvice! ++javascript-init-lsp-or-tide-maybe-h ()
;;   ;;   :override #'+javascript-init-lsp-or-tide-maybe-h
;;   ;;   nil)

;;   ;; fix Error running timer ‘acm-idle-completion’: (void-variable with)
;;   ;; (defadvice! +acm-backend-dabbrev-get-words (word)
;;   ;;   :override #'acm-backend-dabbrev-get-words
;;   ;;   (require 'dabbrev)
;;   ;;   (acm-silent
;;   ;;    (let ((dabbrev-check-other-buffers nil)
;;   ;;          (dabbrev-check-all-buffers nil))
;;   ;;      (dabbrev--reset-global-variables))
;;   ;;    (let ((min-len (+ acm-backend-dabbrev-min-length (length word))))
;;   ;;      (cl-loop for w in (dabbrev--find-all-expansions word (dabbrev--ignore-case-p word))
;;   ;;               if (>= (length w) min-len) collect w))))

;;   (setq lsp-bridge-lang-server-mode-list
;;         '(
;;           ((js2-mode js-mode rjsx-mode) . "javascript")
;;           (typescript-tsx-mode . "typescriptreact")
;;           ((typescript-mode) . "typescript")
;;           ))
;;   ;; (when (modulep! :completion company)
;;   ;;   (setq company-global-modes
;;   ;;         '(not erc-mode
;;   ;;           circe-mode
;;   ;;           message-mode
;;   ;;           help-mode
;;   ;;           gud-mode
;;   ;;           vterm-mode
;;   ;;           emacs-lisp-mode)))
;;   ;; (defun enable-lsp-bridge-for-modes ()
;;   ;;   ;; 只对指定的 mode 开启 lsp-bridge
;;   ;;   (dolist (hook (list
;;   ;;                  'python-mode-hook
;;   ;;                  'typescript-mode-hook
;;   ;;                  'js2-mode-hook
;;   ;;                  'js-mode-hook
;;   ;;                  'rjsx-mode-hook
;;   ;;                  'typescript-tsx-mode-hook
;;   ;;                  'emacs-lisp-mode-hook))
;;   ;;     (add-hook hook (lambda () (lsp-bridge-mode)))))
;;   (global-lsp-bridge-mode))

;; (use-package! lsp-bridge-diagnostics
;;   :after (lsp-bridge)
;;   :config
;;   (setq lsp-bridge-diagnostics-provider :native)
;;   (setq lsp-bridge-diagnostics-disabled-modes '(rust-mode rustic-mode)))

;; (defun lsp-bridge--vue-project-p (project-path)
;;   (if-let ((package-json (f-join project-path "package.json"))
;;            (exist (f-file-p package-json))
;;            (config (json-read-file package-json))
;;            (dependencies (alist-get 'dependencies config)))
;;       (alist-get 'vue dependencies)
;;     nil))

;; (defun my/get-private-langserver (file-name)
;;   (f-join doom-private-dir "langserver" file-name))

;; (setq volar-project-list '(
;;                            "/media/psf/Home/Documents/JadeStrong/olympic-game"
;;                            "/Users/jadestrong/Documents/JadeStrong/ugfe-activity"
;;                            "/Users/jadestrong/Documents/JadeStrong/olympic-game"
;;                            ))
;; (setq vls-project-list '(
;;                          "/Users/jadestrong/Documents/JadeStrong/ugfe-nebula"
;;                          "/Users/jadestrong/Documents/JadeStrong/ugfe-orchard"
;;                          ))
;; (defun lsp-bridge--get-lang-server-by-project (project-path file-path)
;;   "Get lang server by PROJECT-PATH ANF FILE_PATH."
;;   (cond ((and (member project-path vls-project-list)
;;               (string-equal (file-name-extension file-path) "vue"))
;;          (my/get-private-langserver "vls.json"))
;;         ((or (member project-path volar-project-list)
;;              (and (not (member project-path vls-project-list)) (lsp-bridge--vue-project-p project-path)))
;;          (my/get-private-langserver "volar.json"))
;;         ;; ((string-equal (file-name-extension file-path) "rs")
;;         ;;  (my/get-private-langserver "rust-analyzer.json"))
;;         (t nil)))
;; (setq lsp-bridge-get-lang-server-by-project #'lsp-bridge--get-lang-server-by-project)

(use-package! lsp-bridge
  :load-path "~/.doom.d/extensions/lsp-bridge"
  :config
  (map! :map acm-mode-map
        [tab]           #'acm-select-next
        [backtab]       #'acm-select-prev)
  (map! :map doom-leader-code-map
        :desc "LSP Rename"
        "r"             #'lsp-bridge-rename
        :desc "LSP Find declaration"
        "j"             #'lsp-bridge-find-def)
  (require 'yasnippet)
  (yas-global-mode 1)
  (global-lsp-bridge-mode))
