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
      doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 17)
      ivy-posframe-font (font-spec :family "JetBrains Mono" :size 16)

      plantuml-exec-mode 'jar
      plantuml-output-type "png"

      word-wrap t
      word-wrap-by-category t

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      ;; display-line-numbers-type nil

      ;; disable deft auto save
      deft-auto-save-interval 0

      ;; company and company-lsp
      ;; company-minimum-prefix-length 3

      ;; More common use-case
      evil-ex-substitute-global t
      evil-want-fine-undo t
      ;; Switch to the new window after splitting
      evil-split-window-below t
      evil-vsplit-window-right t
      ;; evil-want-minibuffer t
      ;; evil-collection-setup-minibuffer t
      auto-save-default nil
      ;; flycheck-checker-error-threshold nil
      flycheck-highlighting-style `(conditional 10 level-face (delimiters "" ""))
      ;; +lsp-company-backends '(company-tabnine company-capf :with company-yasnippet)
      ;; +lsp-company-backends '(company-capf :with company-tabnine :separate)
      ;; +lsp-company-backends '(company-capf company-yasnippet :with company-tabnine :separate)
      ;; +lsp-company-backends '(:separate company-tabnine-capf)
      ;; company-lsp-proxy
      +lsp-company-backends '(company-capf :separate company-dabbrev)
      +company-backend-alist '((text-mode (:separate company-dabbrev company-yasnippet)) ;; company-ispell is annoying for `Start looking process...` in Chinese
                               (web-mode (company-capf :separate company-dabbrev))
                               (prog-mode (company-capf :separate company-dabbrev)) ;;  company-yasnippet 指定 prog-mode 使用 company-tabnine-capf ，使用 rust-analyzer 服务时这个通过 +lsp-company-backend 指定的后端 revert buffer 后总是会被这个配置的值覆盖
                               (conf-mode company-capf company-dabbrev-code company-yasnippet))
      )

(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Large file optimize
(setq-default bidi-display-reordering nil)
(setq
 bidi-inhibit-bpa t
 long-line-threshold 1000
 large-hscroll-threshold 1000
 syntax-wholeline-max 1000)

(after! company
  (setq company-abort-on-unique-match nil)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-idle-delay 0))
(after! company-box
  (setq company-box-doc-delay 0.1))

(after! corfu
  (setq +corfu-want-minibuffer-completion nil)
  (setq global-corfu-minibuffer nil)
  (setq corfu-auto-delay 0)
  (setq corfu-preselect 'first)
  (setq corfu-preview-current nil)
  (advice-add #'lsp-proxy-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add #'lsp-proxy-completion-at-point :around #'cape-wrap-nonexclusive))
(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.1 . 0.1)))

(after! lsp
  (setq
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
   lsp-eslint-enable t
   lsp-eslint-download-url "https://github.com/jadestrong/lsp-server-binaries/blob/master/dbaeumer.vscode-eslint-2.2.6.vsix?raw=true"
   lsp-vetur-experimental-template-interpolation-service nil
   lsp-enable-suggest-server-download nil
   lsp-enable-indentation nil ;; don't use lsp-format-region as indent-region-function

   lsp-typescript-suggest-auto-imports t
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
   lsp-javascript-display-variable-type-hints t)
  (setq lsp-clients-typescript-plugins
        (vector
         (list :name "@styled/typescript-styled-plugin"
               :location (expand-file-name "~/.config/yarn/global/node_modules/@styled/typescript-styled-plugin/"))))

  (setq lsp-volar-completion-tag-casing "pascalCase")
  (setq lsp-volar-completion-attr-casing "camelCase")

  (setq +format-with-lsp t)
  ;; when enable format with lsp, then disable typescript-language-server format
  ;; only enable eslint-server otherwise use prettier
  (when (and +format-with-lsp lsp-eslint-enable)
    (setq lsp-javascript-format-enable nil)
    (setq lsp-typescript-format-enable nil)
    (setq lsp-eslint-format t))

  (setq lsp-clients-typescript-preferences '(:includePackageJsonAutoImports "on"
                                             :includeAutomaticOptionalChainCompletions t
                                             :useLabelDetailsInCompletionEntries t))
  (setq lsp-clients-typescript-max-ts-server-memory 3072)
  (setq completion-ignore-case t)
  (setq lsp-completion-no-cache nil)
  (setq lsp-clients-typescript-tsserver '(:logVerbosity "off"))
  (defun toggle-lsp-format ()
    (interactive)
    (setq +format-with-lsp (not +format-with-lsp))))

(after! rustic
  (setq rustic-analyzer-command '("/Users/bytedance/.vscode/extensions/rust-lang.rust-analyzer-0.3.2078-darwin-arm64/server/rust-analyzer"))
  ;; Disable it only for rust buffers - doc not auto display in mini buffer
  (setq-hook! 'rustic-mode-hook lsp-signature-auto-activate nil))

(setq treesit-extra-load-path '("/Users/bytedance/Documents/Github/tree-sitter-module/dist"))

;; disalbe magit-diff to highlight the chunk of removed and added
(after! magit
  (setq magit-diff-refine-hunk nil)
  (setq magit-inhibit-save-previous-winconf t
        ;; transient-values '((magit-rebase "--autosquash"))
        ))

;; doomemacs/doomemacs#6261
(after! web-mode
  (set-company-backend! 'web-mode '(company-capf :separate company-dabbrev)))

(after! editorconfig
  ;; Add support doomemacs's typescript-tsx-mode
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

(after! company-box
  (defun company-box-icons--lsp-proxy (candidate)
    (-when-let* ((copilot-item (get-text-property 0 'lsp-proxy--item candidate))
                 (lsp-item (plist-get copilot-item :item))
                 (kind-num (plist-get lsp-item :kind)))
      (alist-get kind-num company-box-icons--lsp-alist)))

  (setq company-box-icons-functions
        (cons #'company-box-icons--lsp-proxy company-box-icons-functions)))


;;; :lang org

(after! org
  (setq org-log-refile 'note)
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
        ))

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

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(set-popup-rule! "^\\* \\(Chez\\|Mit\\) REPL \\*" :side 'right :quit nil :size 0.5 :select nil :modeline t)
(set-popup-rule! "^\\*projectile-files-errors\\*" :side 'bottom :quit t :size 0.3 :select nil)

;;
;;; Keybinds
(defun my/lsp-format-buffer ()
  "Formats the current buffer based on the current mode.
   If in `lsp-proxy-mode', uses `lsp-proxy-format-buffer'.
   If in `lsp-mode', uses `lsp-format-buffer'.
   Otherwise, uses `+format/buffer'."
  (interactive)
  (cond
   (lsp-proxy-mode (call-interactively #'lsp-proxy-format-buffer))
   ;; (lsp-mode (call-interactively #'lsp-format-buffer))
   (t (call-interactively #'+format/buffer))))

(defun my/lsp-execute-code-action ()
  (interactive)
  (cond (lsp-proxy-mode (call-interactively #'lsp-proxy-execute-code-action))
        ;; (lsp-mode (call-interactively 'lsp-execute-code-action))
        ))

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
      :n "m m" #'better-jumper-set-jump
      ;; avy
      :g "M-g g" #'avy-goto-line
      :g "M-g M-g" #'avy-goto-line
      :g "M-]" #'+workspace/switch-right
      :g "M-[" #'+workspace/switch-left
      :g "M-p" #'evil-scroll-page-up
      :g "M-n" #'evil-scroll-page-down
      :g "M-f" #'forward-char
      :g "M-=" #'er/expand-region
      :g "M-," #'gptel-send
      :g "M-;" #'gptel-menu
      :g "M-l" #'my/gptel-find-chat

      :g "C-c C-a" #'mc/mark-all-like-this
      :leader
      "w w" #'ace-window
      "w 1" #'delete-other-windows
      "w 0" #'+workspace/close-window-or-workspace
      ";" #'counsel-M-x
      ;; ":" #'pp-eval-expression
      ;; "e e" #'flycheck-explain-error-at-point
      ;; "c c" #'lsp-proxy-execute-code-action
      "c c" #'my/lsp-execute-code-action
      "c e" #'lsp-proxy-execute-command
      "c f" #'my/lsp-format-buffer
      "c F" #'+format/buffer
      "c r" #'lsp-proxy-rename
      "c b" #'aider-transient-menu
      "a g" #'gptel
      "a s" #'gptel-send
      "a m" #'gptel-menu)

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

(add-hook 'js-mode-hook 'js2-minor-mode)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))


;;; :lang web
(defun adjust-web-mode-padding ()
  (setq web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-comment-style 2
        ;; web-mode-code-indent-offset 4
        ;; web-mode-css-indent-offset 4
        ;; web-mode-markup-indent-offset 2
        ))
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

;;; Customize function
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(ignore-errors
  (require 'ansi-color)
  (defun my/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer))

(defun xwidget-webkit-estimated-load-progress()
  "To fix void-function warning."
  t)


;;; New packages
(use-package! jsdoc)

(use-package! olivetti
  :config
  (setq eww-retrieve-command '("readable"))
  :hook (eww-mode . olivetti-mode))

(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

(use-package! treesit)
(use-package! rust-ts-mode
  :mode ("\\.rs$" . rust-ts-mode))

(use-package! treesit-auto
  :config
  (setq my/js-tsauto-config
        (make-treesit-auto-recipe
         :lang 'javascript
         :ts-mode 'js-ts-mode
         :remap '(js2-mode rjsx-mode js-mode javascript-mode)
         :url "https://github.com/tree-sitter/tree-sitter-javascript"
         :revision "master"
         :source-dir "src"))
  (setq my/tsx-tsauto-config
        (make-treesit-auto-recipe
         :lang 'tsx
         :ts-mode 'tsx-ts-mode
         :remap 'typescript-tsx-mode
         :url "https://github.com/tree-sitter/tree-sitter-typescript"
         :revision "master"
         :source-dir "tsx/src"))
  (add-to-list 'treesit-auto-recipe-list my/js-tsauto-config)
  (add-to-list 'treesit-auto-recipe-list my/tsx-tsauto-config)
  ;; (delete 'rust treesit-auto-langs)
  (global-treesit-auto-mode)
  (advice-add 'treesit-install-language-grammar
              :after (lambda (&rest _r) (treesit-auto-apply-remap))))

(use-package! gptel
  :config
  (setq gptel-model 'deepseek-chat)
  ;; (setq gptel-model "moonshot-v1-8k")
  (setq gptel-default-mode 'markdown-mode)
  (setq gptel-log-level nil)
  (setq gptel-backend (gptel-make-openai "DeepSeek"
    :key 'gptel-api-key
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :models '(deepseek-chat deepseek-coder)))
  (defun my/gptel-find-chat (&optional arg)
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
    (markdown-mode)
    (gptel-mode))

  ;; (setq gptel-backend
  ;;       (gptel-make-openai "Doubao"
  ;;         :key 'gptel-api-key
  ;;         :models '("Doubao-pro-32k" "ep-20250115145004-k4jsr" "ep-20250115172533-ztbwr")
  ;;         :host "ark-cn-beijing.bytedance.net"
  ;;         :stream t
  ;;         :endpoint "/api/v3/chat/completions")
  ;;       ;; (gptel-make-openai "Moonshot"
  ;;       ;;   :key 'gptel-api-key
  ;;       ;;   :models '("moonshot-v1-8k"
  ;;       ;;             "moonshot-v1-32k"
  ;;       ;;             "moonshot-v1-128k")
  ;;       ;;   :host "api.moonshot.cn")
  ;;       )
  )

(use-package! magit-gptcommit
  :demand t
  :after gptel magit
  :config
  ;; Enable magit-gptcommit-mode to watch staged changes and generate commit message automatically in magit status buffer
  ;; This mode is optional, you can also use `magit-gptcommit-generate' to generate commit message manually
  ;; `magit-gptcommit-generate' should only execute on magit status buffer currently
  ;; (magit-gptcommit-mode 1)

  ;; Add gptcommit transient commands to `magit-commit'
  ;; Eval (transient-remove-suffix 'magit-commit '(1 -1)) to remove gptcommit transient commands
  (magit-gptcommit-status-buffer-setup)
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept)))

;; (use-package! lsp-proxy
;;   :config
;;   (set-popup-rule! "^\\*lsp-proxy-\\(help\\|diagnostics\\)" :size 0.35 :quit t :select t)
;;   (add-hook! '(
;;                tsx-ts-mode-hook
;;                js-ts-mode-hook
;;                typescript-mode-hook
;;                typescript-ts-mode-hook
;;                rjsx-mode-hook
;;                less-css-mode-hook web-mode-hook
;;                python-ts-mode-hook
;;                rust-mode-hook
;;                rustic-mode-hook
;;                rust-ts-mode-hook
;;                toml-ts-mode-hook
;;                conf-toml-mode-hook
;;                bash-ts-mode-hook
;;                ) #'lsp-proxy-mode)
;;   (set-lookup-handlers! 'lsp-proxy-mode
;;     :definition '(lsp-proxy-find-definition :async t)
;;     :references '(lsp-proxy-find-references :async t)
;;     :implementations '(lsp-proxy-find-implementations :async t)
;;     :type-definition '(lsp-proxy-find-type-definition :async t)
;;     :documentation '(lsp-proxy-describe-thing-at-point :async t)))
(use-package! lsp-proxy
  :load-path "~/.doom.d/extensions/lsp-proxy"
  :config
  (setq lsp-proxy-log-level 1)
  (setq lsp-proxy-log-max 0)
  (set-popup-rule! "^\\*lsp-proxy-\\(help\\|diagnostics\\)" :size 0.35 :quit t :select t)
  (add-hook! '(
               tsx-ts-mode-hook
               js-ts-mode-hook
               typescript-mode-hook
               typescript-ts-mode-hook
               rjsx-mode-hook
               less-css-mode-hook web-mode-hook
               python-mode-hook
               python-ts-mode-hook
               rust-mode-hook
               rustic-mode-hook
               rust-ts-mode-hook
               toml-ts-mode-hook
               conf-toml-mode-hook
               bash-ts-mode-hook
               dart-mode-hook
               json-mode-hook
               json-ts-mode-hook
               ruby-ts-mode-hook
               ) #'lsp-proxy-mode)
  (setq lsp-proxy-inlay-hints-mode-config '(rust-mode rust-ts-mode tsx-ts-mode typescript-ts-mode))
  ;; (add-hook! '(rust-mode-hook rust-ts-mode-hook) #'lsp-proxy-inlay-hints-mode)
  (setq lsp-proxy--send-changes-idle-time 0)
  (setq lsp-proxy-diagnostics-provider :auto)
  (set-lookup-handlers! 'lsp-proxy-mode
    :definition '(lsp-proxy-find-definition :async t)
    :references '(lsp-proxy-find-references :async t)
    :implementations '(lsp-proxy-find-implementations :async t)
    :type-definition '(lsp-proxy-find-type-definition :async t)
    :documentation '(lsp-proxy-describe-thing-at-point :async t)))

(use-package! demo
  :load-path "~/.doom.d/extensions/demo-jsonrpc")

;; (use-package eglot-lsp-proxy
;;   :load-path "~/.doom.d/extensions/eglot-lsp-proxy"
;;   :after eglot
;;   :config (eglot-proxy-mode))

;; (use-package! tokio-jsonrpc
;;   :load-path "~/.doom.d/extensions/tokio-jsonrpc-demo")

;; (use-package! tabnine
;;   :hook (
;;          ;; (prog-mode . tabnine-mode)
;; 	 (kill-emacs . tabnine-kill-process))
;;   :config
;;   (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
;;   (tabnine-start-process)
;;   :bind
;;   (:map  tabnine-completion-map
;; 	 ("<tab>" . tabnine-accept-completion)
;; 	 ("TAB" . tabnine-accept-completion)
;; 	 ("M-f" . tabnine-accept-completion-by-word)
;; 	 ("M-<return>" . tabnine-accept-completion-by-line)
;; 	 ("C-g" . tabnine-clear-overlay)
;; 	 ("M-[" . tabnine-previous-completion)
;; 	 ("M-]" . tabnine-next-completion)))


;; utils

(defun toggle-so-long-mode ()
  "Clear."
  (interactive)
  (setq-local doom-large-file-p nil)
  (revert-buffer))

(defun goto-download-dir ()
  (interactive)
  (dired target-dir-path))

(defun open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun rust-save-compile-and-run ()
  (interactive)
  (save-buffer)

  (if (locate-dominating-file (buffer-file-name) "Cargo.toml")

      (compile "cargo run")

    (compile
     (format "rustc %s && %s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name))))))

(defun my/ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))
