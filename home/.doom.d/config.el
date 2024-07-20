;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "+roam")
(load! "+hacks")
(load! "+leetcode")
(load! "+treesit")
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
      lsp-rust-analyzer-cargo-load-out-dirs-from-check t ;; support extern C suggest
      lsp-rust-analyzer-proc-macro-enable t ;; same above

      lsp-rust-analyzer-experimental-proc-attr-macros t ;; 内嵌变量提示
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-max-inlay-hint-length 25

      lsp-javascript-display-inlay-hints nil
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
      ;; company-lsp-copilot
      +lsp-company-backends '(company-capf :separate company-dabbrev)
      +company-backend-alist '((text-mode (:separate company-dabbrev company-yasnippet)) ;; company-ispell is annoying for `Start looking process...` in Chinese
                               ;; company-lsp-copilot company-lsp-rocks
                               (prog-mode company-capf :separate company-dabbrev) ;;  company-yasnippet 指定 prog-mode 使用 company-tabnine-capf ，使用 rust-analyzer 服务时这个通过 +lsp-company-backend 指定的后端 revert buffer 后总是会被这个配置的值覆盖
                               ;; (prog-mode company-capf) ;;  company-yasnippet 指定 prog-mode 使用 company-tabnine-capf ，使用 rust-analyzer 服务时这个通过 +lsp-company-backend 指定的后端 revert buffer 后总是会被这个配置的值覆盖
                               (conf-mode company-capf company-dabbrev-code company-yasnippet))
      )

;; (setq company-idle-delay 0)
(setq company-abort-on-unique-match nil)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-idle-delay 0)
(setq rustic-analyzer-command '("/Users/bytedance/.vscode/extensions/rust-lang.rust-analyzer-0.3.1940-darwin-arm64/server/rust-analyzer"))
(setq company-box-doc-delay 0.1)
(setq treesit-extra-load-path '("/Users/bytedance/Documents/Github/tree-sitter-module/dist"))
(setq +format-with-lsp t)

;; when enable format with lsp, then disable typescript-language-server format
;; only enable eslint-server otherwise use prettier
(when (and +format-with-lsp lsp-eslint-enable)
  (setq lsp-javascript-format-enable nil)
  (setq lsp-typescript-format-enable nil)
  (setq lsp-eslint-format t))

(setq lsp-clients-typescript-preferences '((includePackageJsonAutoImports . "on")
                                           (includeAutomaticOptionalChainCompletions . t)))
(setq lsp-clients-typescript-max-ts-server-memory 3072)
(setq completion-ignore-case t)
(setq lsp-completion-no-cache nil)

;; (setq lsp-clients-typescript-tsserver '((logVerbosity . "verbose")))

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

;; (setq inhibit-message nil)
;; (setq gif-screencast-scale-factor 2)

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
        mac-option-modifier 'meta
        mac-right-option-modifier 'meta)
  (defconst target-dir-path "~/Downloads/" "My Download directory")
  (setq create-lockfiles nil)) ;; backup file

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
(set-popup-rule! "^\\*projectile-files-errors\\*" :side 'bottom :quit t :size 0.3 :select nil)

;;
;;; Keybinds
(defun my/lsp-format-buffer ()
  "Formats the current buffer based on the current mode.
   If in `lsp-copilot-mode', uses `lsp-copilot-format-buffer'.
   If in `lsp-mode', uses `lsp-format-buffer'.
   Otherwise, uses `+format/buffer'."
  (interactive)
  (cond
   (lsp-copilot-mode (lsp-copilot-format-buffer))
   (lsp-mode (lsp-format-buffer))
   (t (+format/buffer))))

(defun my/lsp-execute-code-action ()
  (interactive)
  (cond (lsp-copilot-mode (lsp-copilot-execute-code-action))
        (lsp-mode (lsp-execute-code-action))))

(map! :n [tab] (cmds! (and (modulep! :editor fold)
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
      :n "[ e" #'flycheck-previous-error
      :n "] e" #'flycheck-next-error
      :n "M-n" #'evil-scroll-page-down
      :n "M-p" #'evil-scroll-page-up
      :n "M-i" #'switch-to-prev-buffer
      :n "M-o" #'switch-to-next-buffer
      ;; avy
      :g "M-g g" #'avy-goto-line
      :g "M-g M-g" #'avy-goto-line
      :g "M-]" #'+workspace/switch-right
      :g "M-[" #'+workspace/switch-left
      :g "M-p" #'evil-scroll-page-up
      :g "M-n" #'evil-scroll-page-down
      :g "M-f" #'forward-char
      :leader
      "w w" #'ace-window
      "w 1" #'delete-other-windows
      "w 0" #'+workspace/close-window-or-workspace
      ";" #'counsel-M-x
      ;; ":" #'pp-eval-expression
      ;; "e e" #'flycheck-explain-error-at-point
      ;; "c c" #'lsp-copilot-execute-code-action
      "c c" #'my/lsp-execute-code-action
      "c e" #'lsp-copilot-execute-command
      "c f" #'my/lsp-format-buffer)

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
;; (map! :map (js2-mode-map typescript-mode-map typescript-tsx-mode-map tsx-ts-mode-map typescript-ts-mode-map)
;;       "C-c j" 'js-doc-insert-function-doc
;;       "@" 'js-doc-insert-tag)

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

(use-package! dirvish
  :config
  (dirvish-override-dired-mode 1)
  (setq dirvish-depth 0)
  (setq dirvish-preview-dispatchers (remove 'directory-exa dirvish-preview-dispatchers))
  (setq! dirvish-attributes '(file-size)) ;; all-the-icons
  (map! :map dirvish-mode-map
        :n "M-f" #'dirvish))
(after! diredfl
  (custom-theme-set-faces
   'user
   '(dirvish-hl-line ((t (:inherit 'diredfl-flag-mark))))))

;; (use-package! citre
;;   :when (modulep! :completion company)
;;   :defer t
;;   :init
;;   (require 'citre-config)
;;   (global-set-key (kbd "C-x c j") 'citre-jump)
;;   (global-set-key (kbd "C-x c J") 'citre-jump-back)
;;   (global-set-key (kbd "C-x c p") 'citre-ace-peek)
;;   :config
;;   (setq citre-project-root-function #'projectile-project-root)
;;   ;; See https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
;;   (define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
;;     (let ((fetcher (apply -fn -args))
;;           (citre-fetcher
;;            (let ((xref-backend-functions '(citre-xref-backend t)))
;;              (apply -fn -args))))
;;       (lambda ()
;;         (or (with-demoted-errors "%s, fallback to citre"
;;               (funcall fetcher))
;;             (funcall citre-fetcher))))))

;; (use-package! gif-screencast
;;   :defer
;;   :config
;;   (setq gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
;;   (setq gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
;;   (setq gif-screencast-capture-format "ppm") ;; Optional: Required to crop captured images.
;;   )

;; (use-package! emacs-baidupan)

;;; Language customizations

;; evil-matchit 只在 web-mode 和 html-mode 下开启这个 mode ，因为它在 js 等 mode 下有 bug
;; 使用它主要解决 doom-emacs 自带的 % 功能不支持 html 标签匹配跳转
(defun evilmi-jtsx-get-tag ()
  "Find the jsx element tag at point's pair tag."
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (let ((start (treesit-node-start enclosing-element))
              (end (treesit-node-end enclosing-element)))
          (if (> (point) (+ start (/ (- end start) 2)))
              (list end 1) ; We are closer to the closing tag.
            (list start 0))) ; We are closer to the opening tag.
      nil)))
(defun evilmi-jtsx-jump (info _num)
  "Jump to the pair tag poision."
  (let* ((tag-type (nth 1 info)))
    (cond
     ((eq 1 tag-type)
      (jtsx-jump-jsx-opening-tag))
     ((eq 0 tag-type)
      (jtsx-jump-jsx-closing-tag)))
    (point)))
(use-package! evil-matchit-mode
  :hook (web-mode html-mode tsx-ts-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html))
  (evilmi-load-plugin-rules '(tsx-ts-mode) '(simple javascript jtsx html)))
;; (setq evilmi-debug t)

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

(defun xwidget-webkit-estimated-load-progress()
  "To fix void-function warning."
  t)

(use-package! xwidget-webkit-vimium)
(map! :map xwidget-webkit-mode-map
      :n "f" 'xwidget-webkit-vimium-get-candidates
      :n "e" 'evil-collection-xwidget-webkit-scroll-half-page-down
      :n "S" 'xwidget-webkit-back)

(after! lsp-mode
  (setq lsp-clients-typescript-plugins
        (vector
         (list :name "@styled/typescript-styled-plugin"
               :location (expand-file-name "~/.config/yarn/global/node_modules/@styled/typescript-styled-plugin/")))))


(setq magit-git-executable "/usr/bin/git")
;; (use-package! lsp-tailwindcss
;;   :when (modulep! :tools lsp)
;;   :init
;;   (setq lsp-tailwindcss-add-on-mode t)
;;   :config
;;   (add-to-list 'lsp-tailwindcss-major-modes 'tsx-ts-mode))

(use-package! jsdoc)

(setq eww-retrieve-command '("readable"))
(use-package! olivetti
  :hook (eww-mode . olivetti-mode))

;; (use-package! epc)
(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(defun company-box-icons--lsp-rocks (candidate)
  (-when-let* ((lsp-item (get-text-property 0 'lsp-rocks--item candidate))
               (kind-num (plist-get lsp-item :kind)))
    (alist-get kind-num company-box-icons--lsp-alist)))

(defun company-box-icons--lsp-copilot (candidate)
  (-when-let* ((copilot-item (get-text-property 0 'lsp-copilot--item candidate))
               (lsp-item (plist-get copilot-item :item))
               (kind-num (plist-get lsp-item :kind)))
    (alist-get kind-num company-box-icons--lsp-alist)))

(after! company-box
  (setq company-box-icons-functions
        (cons #'company-box-icons--lsp-copilot (cons #'company-box-icons--lsp-rocks company-box-icons-functions))))

(defun my-ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun table-print (table)
  (json-read-from-string (json-encode table)))

(use-package! treesit)
(after! rust-ts-mode
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("\\.rs\\'" . rust-mode) auto-mode-alist)))

(use-package! treesit-auto
  :config
  ;; (defadvice! +treesit-auto--remap-language-source (language-source)
  ;;   :override #'treesit-auto--remap-language-source
  ;;   (let* ((name (car language-source))
  ;;          (name-ts-mode (intern (concat (symbol-name name) "-ts-mode")))
  ;;          (fallback-assoc (assq name-ts-mode treesit-auto-fallback-alist))
  ;;          (fallback-name (cdr fallback-assoc))
  ;;          (name-mode (or fallback-name
  ;;                         (intern (concat (symbol-name name) "-mode"))))
  ;;          (name-mode-bound-p (fboundp name-mode))
  ;;          (skip-remap-p (and fallback-assoc
  ;;                             (not (cdr fallback-assoc)))))
  ;;     (message "%s: %s" name (treesit-ready-p name t))
  ;;     (and (not skip-remap-p)
  ;;          (fboundp name-ts-mode)
  ;;          (if (treesit-ready-p name t)
  ;;              (add-to-list 'major-mode-remap-alist `(,name-mode . ,name-ts-mode))
  ;;            (when name-mode-bound-p
  ;;              (add-to-list 'major-mode-remap-alist `(,name-ts-mode . ,name-mode)))))))
  ;; (setq treesit-auto-fallback-alist (assoc-delete-all 'tsx-ts-mode treesit-auto-fallback-alist))
  ;; (add-to-list 'treesit-auto-fallback-alist '(tsx-ts-mode . typescript-tsx-mode))
  ;; (add-to-list 'treesit-auto-fallback-alist '(js-ts-mode . rjsx-mode))
  (setq my-js-tsauto-config
        (make-treesit-auto-recipe
         :lang 'javascript
         :ts-mode 'js-ts-mode
         :remap '(js2-mode rjsx-mode js-mode javascript-mode)
         :url "https://github.com/tree-sitter/tree-sitter-javascript"
         :revision "master"
         :source-dir "src"))
  (setq my-tsx-tsauto-config
        (make-treesit-auto-recipe
         :lang 'tsx
         :ts-mode 'tsx-ts-mode
         :remap 'typescript-tsx-mode
         :url "https://github.com/tree-sitter/tree-sitter-typescript"
         :revision "master"
         :source-dir "tsx/src"))
  (add-to-list 'treesit-auto-recipe-list my-js-tsauto-config)
  (add-to-list 'treesit-auto-recipe-list my-tsx-tsauto-config)
  ;; (delete 'rust treesit-auto-langs)
  (global-treesit-auto-mode)
  (advice-add 'treesit-install-language-grammar
              :after (lambda (&rest _r) (treesit-auto-apply-remap))))

(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

(use-package! mind-wave
  :load-path "~/.doom.d/extensions/mind-wave"
  :config
  (setq mind-wave-api-base "https://oa.api2d.net/v1")
  (defun z/mind-wave-find-chat (&optional arg)
    "Creat new chat. with ARG, find previous chat."
    (interactive "P")
    (find-file
     (if arg
         (completing-read
          "Choose chat: "
          (cl-remove-if
           (lambda (x) (member x '("." "..")))
           (directory-files (locate-user-emacs-file "mind-wave") t "\\.chat\\'")))
       (concat user-emacs-directory "mind-wave/" (format-time-string "%FT%T") ".chat")))
    (mind-wave-chat-mode)))

;; (add-hook! '(js-ts-mode-local-vars-hook)
;;            #'lsp!)

;; (use-package! md-preview
;;   :load-path "~/.doom.d/extensions/md-preview"
;;   :config
;;   (setq epc:debug-out nil))
;; (setq lsp-rocks-epc--debug-out nil)

;; (use-package! lsp-rocks
;;   :load-path "~/.doom.d/extensions/lsp-rocks"
;;   :config
;;   ;; (add-hook 'tsx-ts-mode-hook #'lsp-rocks-mode)
;;   (add-hook! '(tsx-ts-mode-hook js-ts-mode-hook typescript-ts-mode-hook) #'lsp-rocks-mode)
;;   (set-lookup-handlers! 'lsp-rocks-mode
;;     :definition #'lsp-rocks-find-definition
;;     :documentation '(lsp-rocks-describe-thing-at-point)))

(set-popup-rule! "^\\*lsp-copilot-\\(help\\|diagnostics\\)" :size 0.35 :quit t :select t)
(use-package! lsp-copilot
  :load-path "~/.doom.d/extensions/lsp-copilot"
  :config
  (add-hook! '(tsx-ts-mode-hook js-ts-mode-hook typescript-ts-mode-hook less-css-mode-hook web-mode-hook) #'lsp-copilot-mode)
  (set-lookup-handlers! 'lsp-copilot-mode
    :definition '(lsp-copilot-find-definition :async t)
    :documentation '(lsp-copilot-describe-thing-at-point :async t)))

;; (use-package! lspce
;;   :load-path "~/.doom.d/extensions/lspce"
;;   :config
;;   (setq lspce-send-changes-idle-time 1)
;;   (lspce-set-log-file "/tmp/lspce.log")
;;   (add-hook! '(tsx-ts-mode-hook typescript-ts-mode-hook rustic-mode-hook) #'lspce-mode)
;;   (setq lspce-server-programs `(("rust"  "/Users/bytedance/.vscode/extensions/rust-lang.rust-analyzer-0.3.1657-darwin-arm64/server/rust-analyzer" "" lspce-ra-initializationOptions)
;;                                 ("rustic"  "/Users/bytedance/.vscode/extensions/rust-lang.rust-analyzer-0.3.1657-darwin-arm64/server/rust-analyzer" "" lspce-ra-initializationOptions)
;;                                 ("typescript" "typescript-language-server" "--stdio")
;;                                 ("js" "typescript-language-server" "--stdio")
;;                                 ("js" "vscode-eslint-language-server" "--stdio")
;;                                 ("tsx" "typescript-language-server" "--stdio")
;;                                 ("tsx" "vscode-eslint-language-server" "--stdio")
;;                                 ;; ("python" "pylsp" "" )
;;                                 ;; ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
;;                                 ;; ("java" "java" lspce-jdtls-cmd-args lspce-jdtls-initializationOptions)
;;                                 ))
;; )


;; (use-package! auto-save
;;   :config
;;   (auto-save-enable)
;;   (setq auto-save-silent t)


(setq plantuml-exec-mode 'jar)
(setq plantuml-output-type "png")

(defun toggle-so-long-mode ()
  "Clear."
  (interactive)
  (setq-local doom-large-file-p nil)
  (revert-buffer))

(setq lsp-signature-function 'lsp-signature-posframe)
;; (setq lsp-signature-auto-activate)
