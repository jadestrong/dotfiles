;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+roam")
(load! "+hacks")
(load! "+leetcode")
;; (load! "+xwwp")

(setq user-full-name "JadeStrong"
      user-mail-address "jadestrong@163.com"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-one

      word-wrap t
      word-wrap-by-category t

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      ;; display-line-numbers-type nil

      ;; lsp
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-ui-sideline-show-code-actions nil
      lsp-enable-symbol-highlighting nil
      lsp-ui-doc-enable nil
      lsp-auto-guess-root t
      read-process-output-max (* 1024 1024)
      lsp-eldoc-render-all nil
      lsp-clients-typescript-log-verbosity "off"
      lsp-eslint-enable t
      lsp-eslint-download-url "https://github.com/jadestrong/lsp-server-binaries/blob/master/dbaeumer.vscode-eslint-2.2.6.vsix?raw=true"
      ;; lsp-eslint-server-command `("node" "/User/jadestrong/.vscode/extensions/dbaeumer.vscode-eslint-2.2.2/server/out/eslintServer.js" "--stdio")
      lsp-vetur-experimental-template-interpolation-service nil
      lsp-enable-suggest-server-download nil
      lsp-enable-indentation nil ;; don't use lsp-format-region as indent-region-function

      lsp-typescript-suggest-auto-imports t

      ;; disable deft auto save
      deft-auto-save-interval 0

      ;; company and company-lsp
      ;; company-minimum-prefix-length 3

      ;; rust
      rustic-lsp-server 'rust-analyzer
      rustic-analyzer-command '()
      lsp-rust-analyzer-cargo-load-out-dirs-from-check t ;; support extern C suggest
      lsp-rust-analyzer-proc-macro-enable t ;; same above

      lsp-rust-analyzer-experimental-proc-attr-macros t ;; 内嵌变量提示
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-max-inlay-hint-length 25

      lsp-javascript-display-inlay-hints t
      lsp-javascript-display-enum-member-value-hints t
      lsp-javascript-display-return-type-hints t
      lsp-javascript-display-parameter-type-hints t
      lsp-javascript-display-parameter-name-hints-when-argument-matches-name t
      lsp-javascript-display-property-declaration-type-hints t
      lsp-javascript-display-variable-type-hints t

      ;; More common use-case
      evil-ex-substitute-global t
      evil-want-fine-undo t
      ;; evil-want-minibuffer t
      ;; evil-collection-setup-minibuffer t
      auto-save-default nil
      ;; flycheck-checker-error-threshold nil
      flycheck-highlighting-style `(conditional 10 level-face (delimiters "" ""))
      ;; +lsp-company-backends '(company-tabnine company-capf :with company-yasnippet)
      ;; +lsp-company-backends '(company-capf :with company-tabnine :separate)
      ;; +lsp-company-backends '(company-capf company-yasnippet :with company-tabnine :separate)
      ;; +lsp-company-backends '(:separate company-tabnine-capf)
      +lsp-company-backends '(company-capf :separate company-dabbrev)
      +company-backend-alist '((text-mode (:separate company-dabbrev company-yasnippet)) ;; company-ispell is annoying for `Start looking process...` in Chinese
                               (prog-mode company-capf) ;;  company-yasnippet 指定 prog-mode 使用 company-tabnine-capf ，使用 rust-analyzer 服务时这个通过 +lsp-company-backend 指定的后端 revert buffer 后总是会被这个配置的值覆盖
                               (conf-mode company-capf company-dabbrev-code company-yasnippet))
      )

(setq rust-analyzer-command-path (expand-file-name "~/.vscode/extensions/rust-lang.rust-analyzer-0.3.1115-darwin-x64/server/rust-analyzer"))
(push rust-analyzer-command-path rustic-analyzer-command)

(setq +format-with-lsp t)
;; when enable format with lsp, then disable typescript-language-server format
;; only enable eslint-server otherwise use prettier
(when (and +format-with-lsp lsp-eslint-enable)
  (setq lsp-javascript-format-enable nil)
  (setq lsp-typescript-format-enable nil)
  (setq lsp-eslint-format t))

(setq lsp-clients-typescript-preferences '((includePackageJsonAutoImports . "auto")
                                           (includeAutomaticOptionalChainCompletions . t)))
(setq lsp-clients-typescript-max-ts-server-memory 3072)
(setq completion-ignore-case t)
;; disalbe magit-diff to highlight the chunk of removed and added
(after! magit
  (setq magit-diff-refine-hunk nil))

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode
                                                 (web-mode-indent-style lambda (size) 2)
                                                 web-mode-attr-indent-offset
                                                 web-mode-attr-value-indent-offset
                                                 web-mode-code-indent-offset
                                                 web-mode-css-indent-offset
                                                 web-mode-markup-indent-offset
                                                 web-mode-sql-indent-offset
                                                 web-mode-block-padding
                                                 web-mode-script-padding
                                                 web-mode-style-padding
                                                 typescript-indent-level)))


(defun toggle-lsp-format ()
  (interactive)
  (setq +format-with-lsp (not +format-with-lsp)))

(setq inhibit-message nil)
(setq gif-screencast-scale-factor 2)

;;; :lang org
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "roam/")
      ;; org-roam-directory "~/.roam"
      deft-directory (concat org-directory "deft/")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " ▼ "
      org-attach-id-dir (concat org-roam-directory ".attach/")
      ;; org-superstar-headline-bullets-list '("#")
      )

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)
  (defconst target-dir-path "~/Downloads/" "My Download directory")
  (setq create-lockfiles t))

(when IS-LINUX
  (setq x-super-keysym 'meta)
  (setq x-meta-keysym 'super)
  (defconst target-dir-path "/media/psf/Home/Downloads/" "My Download directory")
  (setq org-directory "/media/psf/Home/org/")
  (setq org-roam-directory (concat org-directory "roam")))

(setq lsp-log-io nil)

(setq lsp-volar-completion-tag-casing "pascalCase")
(setq lsp-volar-completion-attr-casing "camelCase")
(setq lsp-volar-inlay-hints t)

;; emacs-29
;;(when (version= emacs-version "29.0.50")
;;  (general-auto-unbind-keys :off)
;;  (remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)
;;  (set-face-attribute 'mode-line-active nil :inherit 'mode-line))

;;
;;; UI


;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 17)
      ivy-posframe-font (font-spec :family "JetBrains Mono" :size 16))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(set-popup-rule! "^\\* \\(Chez\\|Mit\\) REPL \\*" :side 'right :quit nil :size 0.5 :select nil :modeline t)


;;
;;; Keybinds

(map! :n [tab] (cmds! (and (featurep! :editor fold)
                           (save-excursion (end-of-line) (invisible-p (point))))
                      #'+fold/toggle
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :v [tab] (cmds! (and (bound-and-true-p yas-minor-mode)
                           (or (eq evil-visual-selection 'line)
                               (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                      #'yas-insert-snippet
                      (fboundp 'evil-jump-item)
                      #'evil-jump-item)
      :n "M-." #'lsp-execute-code-action
      :n "[ e" #'flycheck-previous-error
      :n "] e" #'flycheck-next-error
      :n "M-n" #'evil-scroll-page-down
      :n "M-p" #'evil-scroll-page-up
      :n "M-i" #'switch-to-prev-buffer
      :n "M-o" #'switch-to-next-buffer
      ;; avy
      :g "M-g g" #'avy-goto-line
      :g  "M-g M-g" #'avy-goto-line
      :g "s-n" #'+workspace/switch-right
      :g "s-p" #'+workspace/switch-left
      :leader
      "w w" #'ace-window
      "w 1" #'delete-other-windows
      "w 0" #'+workspace/close-window-or-workspace
      ";" #'counsel-M-x
      ":" #'pp-eval-expression
      "e e" #'flycheck-explain-error-at-point)


;; company
(map! (:after company
       :map company-active-map
       "M-n" #'company-select-next-or-abort
       "M-p" #'company-select-previous-or-abort
       :map company-search-map
       "M-n" #'company-select-next-or-abort
       "M-p" #'company-select-previous-or-abort))

;; ivy + minibuffer
(map! :map minibuffer-mode-map
      "M-n" #'ivy-next-line
      "M-p" #'ivy-previous-line)
(map! (:after ivy
       :map ivy-minibuffer-map
       "RET" #'ivy-alt-done
       "M-n" #'ivy-next-line
       "M-p" #'ivy-previous-line
       "M-j" #'+rime-convert-string-at-point
       :map swiper-isearch-map
       "M-n" #'ivy-next-line))

;; rust
(map! (:after rustic
       :map rustic-mode-map
       :localleader
       (:prefix ("r" . "reload")
        :desc "lsp workspace reload" "r" #'lsp-rust-analyzer-reload-workspace)
       (:prefix ("e" . "edit")
        (:desc "cargo add" "a" #'rustic-cargo-add))))

;; org
(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line)))
       :i "M-j" #'+rime-convert-string-at-point))
(map! (:map org-capture-mode-map :n "M-c M-c" #'org-capture-finalize)
      (:map with-editor-mode-map :n "M-c M-c" #'with-editor-finish)
      (:map wdired-mode-map :n "M-c M-c" #'wdired-finish-edit))

;;; Modules

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :lang javascript
(map! :map (js2-mode-map typescript-mode-map typescript-tsx-mode-map)
      "C-c j" 'js-doc-insert-function-doc
      "@" 'js-doc-insert-tag)

(add-hook 'js-mode-hook 'js2-minor-mode)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))

;;; :tools magit
(setq magit-inhibit-save-previous-winconf t
      ;; transient-values '((magit-rebase "--autosquash"))
      )


;;; :lang web
(defun adjust-web-mode-padding ()
  ;; (message "adjust-web-mode-padding")
  (setq web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-comment-style 2
        ;; web-mode-code-indent-offset 4
        ;; web-mode-css-indent-offset 4
        ;; web-mode-markup-indent-offset 2
        )
  )
(add-hook! web-mode #'adjust-web-mode-padding)
(add-hook 'editorconfig-after-apply-functions
           (lambda (props) (adjust-web-mode-padding)))

;; 同时支持 lsp 和 javascript-eslint
(defun creature/lsp-eslint-checker-init ()
  (when (and (not lsp-eslint-enable) ;; when not enable lsp-eslint then add javascript-eslint as next checker
             flycheck-mode
             (flycheck-valid-checker-p 'lsp)
             (flycheck-valid-checker-p 'javascript-eslint))
    (make-local-variable 'flycheck-checkers)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)))
(with-eval-after-load 'lsp-diagnostics
  (add-hook! lsp-diagnostics-mode #'creature/lsp-eslint-checker-init))

(use-package! eslintd-fix
  :commands eslintd-fix
  :config
  (setq-hook! 'eslintd-fix-mode-hook
    flycheck-javascript-eslint-executable eslintd-fix-executable))

;; (use-package! lsp-volar)

(use-package! org-modern
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

(after! org
  (setq org-log-refile 'note))

(use-package! org-alert
  :after org
  :config
  (setq alert-default-style 'osx-notifier)
  (org-alert-enable))

(use-package! dirvish
  :config
  ;; (dirvish-override-dired-mode 1)
  (setq dirvish-depth 0)
  (setq dirvish-preview-dispatchers (remove 'directory-exa dirvish-preview-dispatchers))
  (setq! dirvish-attributes '(file-size)) ;; all-the-icons
  (map! :map dirvish-mode-map
        :n "M-f" #'dirvish-toggle-fullscreen))
(after! diredfl
  (custom-theme-set-faces
   'user
   '(dirvish-hl-line ((t (:inherit 'diredfl-flag-mark))))))

(use-package! citre
  :when (featurep! :completion company)
  :defer t
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  :config
  (setq citre-project-root-function #'projectile-project-root)
  ;; See https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
  (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
    (let ((fetcher (apply -fn -args))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (apply -fn -args))))
      (lambda ()
        (or (with-demoted-errors "%s, fallback to citre"
              (funcall fetcher))
            (funcall citre-fetcher))))))

(use-package! gif-screencast
  :config
  (setq gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (setq gif-screencast-capture-format "ppm") ;; Optional: Required to crop captured images.
  )

;; (use-package! emacs-baidupan)

;;; Language customizations

;; evil-matchit 只在 web-mode 和 html-mode 下开启这个 mode ，因为它在 js 等 mode 下有 bug
;; 使用它主要解决 doom-emacs 自带的 % 功能不支持 html 标签匹配跳转
(use-package! evil-matchit-mode
  :hook (web-mode html-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html)))

;; Disable it only for rust buffers - doc not auto display in mini buffer
(setq-hook! 'rustic-mode-hook lsp-signature-auto-activate nil)

;; 支持拼音搜索中文文件
;; (use-package! pinyinlib)
;; (defun completion--regex-pinyin (str)
;;   (orderless-regexp (pinyinlib-build-regexp-string str)))
;; (after! orderless
;;  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))



;;; Customize function
(defun goto-download-dir ()
  (interactive)
  (dired target-dir-path))

(defun open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; 检查一个 buffer 是否为空
(defun buffer-empty-p (&optional buffer)
  (= (buffer-size buffer) 0))

(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)

  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")

      (compile "cargo run")

    (compile
     (format "rustc %s && %s"
         (buffer-file-name)
         (file-name-sans-extension (buffer-file-name))))))

(use-package! xwidget-webkit-vimium)
(map! :map xwidget-webkit-mode-map
      :n "f" 'xwidget-webkit-vimium-get-candidates
      :n "e" 'evil-collection-xwidget-webkit-scroll-half-page-down
      :n "S" 'xwidget-webkit-back)

;; (use-package! emacs-async)
;; (use-package! greeting
;;   :init
;;   (add-to-list 'load-path (f-join doom-private-dir "extensions/greeting/target/debug"))
;;   (require 'greeting))

;; (defmacro lsp-enable-async (&rest body)
;;   "Enable async."
;;   `(async-start (lambda ()
;;                    (add-to-list 'load-path "/Users/jadestrong/.doom.d/extensions/greeting/target/debug")
;;                    (require 'greeting)
;;                    ,@body
;;                    222)
;;                 (lambda (result)
;;                   (message "Async process done %s." result))))
;; (lsp-enable-async
;;  (greeting-say-hello "emacs"))

;; (use-package! lspce
;;   ;; :load-path (f-join doom-private-dir "extensions/lspce/")
;;   :init
;;   (add-to-list 'load-path (expand-file-name (concat doom-private-dir "extensions/lspce/target/debug")))
;;   (require 'lspce-module)
;;   :config
;;   (setq lspce-send-changes-idle-time 1)
;;   (add-hook 'rust-mode-hook 'lspce-mode)
;;   (setq lspce-server-programs `(("rust-mode" "/Users/jadestrong/.vscode/extensions/rust-lang.rust-analyzer-0.3.1083-darwin-x64/server/rust-analyzer" "" lspce-ra-initializationOptions)))
;;   )

(after! lsp-mode
  (setq lsp-clients-typescript-plugins
        (vector
         (list :name "typescript-styled-plugin"
               :location (expand-file-name "~/.config/yarn/global/node_modules/typescript-styled-plugin/")))))

(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

;; (use-package! jsdoc)

;; (use-package! tsx-mode)

(use-package! apheleia)
