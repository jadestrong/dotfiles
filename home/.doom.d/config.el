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
  (setq rustic-analyzer-command '("~/.vscode/extensions/rust-lang.rust-analyzer-0.3.2078-darwin-arm64/server/rust-analyzer"))
  ;; Disable it only for rust buffers - doc not auto display in mini buffer
  (setq-hook! 'rustic-mode-hook lsp-signature-auto-activate nil))

(setq treesit-extra-load-path '("~/Documents/Github/tree-sitter-module/dist"))

;; disalbe magit-diff to highlight the chunk of removed and added
(after! magit
  (setq magit-diff-refine-hunk t)
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

(map!
 ;; :n [tab] (cmds! (and (modulep! :editor fold)
 ;;                           (save-excursion (end-of-line) (invisible-p (point))))
 ;;                      #'+fold/toggle
 ;;                      (bound-and-true-p code-review-mode)
 ;;                      #'magit-section-toggle
 ;;                      (fboundp 'evil-jump-item)
 ;;                      #'evil-jump-item)
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
 :g "M-=" #'expreg-expand
 :g "M-," #'gptel-send
 :g "M-;" #'gptel-menu
 :g "M-l" #'my/gptel-find-chat
 :g "M-c" #'ignore

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
 "c r" #'lsp-proxy-rename)

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

(map! :leader
      (:prefix ("o" . "open")
               (:prefix ("c" . "claude-code")
                :desc "Start Claude" "c" #'claude-code
                :desc "Start in directory" "d" #'claude-code-start-in-directory
                :desc "Continue the previous one" "C" #'claude-code-continue
                :desc "Resume a Claude session" "r" #'claude-code-resume
                :desc "Create a new instance" "i" #'claude-code-new-instance
                :desc "Kill session" "k" #'claude-code-kill
                :desc "Kill all sessions" "K" #'claude-code-kill-all
                :desc "Send command to Claude" "s" #'claude-code-send-command
                :desc "Send file and line context" "x" #'claude-code-send-command-with-context
                :desc "Send region to Claude" "r" #'claude-code-send-region
                :desc "Send file to Claude" "o" #'claude-code-send-file
                :desc "Fix the error at the point" "e" #'claude-code-fix-error-at-point
                :desc "Fork conversation" "f" #'claude-code-fork
                :desc "Access slash commands menu" "/" #'claude-code-slash-commands
                :desc "Toggle Claude window" "t" #'claude-code-toggle
                :desc "Swith to the Claude buffer" "b" #'claude-code-switch-to-buffer
                :desc "Select and switch a buffer" "B" #'claude-code-select-buffer
                :desc "Toggle read-only mode" "z" #'claude-code-toggle-read-only-mode
                :desc "Send Shift-Tab key" "M" #'claude-code-cycle-mode
                :desc "Send return key" "M" #'claude-code-send-return
                :desc "Send escape key" "n" #'claude-code-send-escape
                :desc "Send 1" "1" #'claude-code-send-1
                :desc "Send 2" "2" #'claude-code-send-2
                :desc "Send 3" "3" #'claude-code-send-3)
               :desc "Open with VSCode" "v" #'open-with-vscode
               :desc "Open with VSCode" "z" #'open-with-zed))

;;; Modules

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))

;; (advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))


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

(defvar evilmi-tsx-treesit--bracket-pairs
  '(("(" . ")")
    ("{" . "}")
    ("[" . "]")))

(defun evilmi-tsx-treesit--on-bracket-p (node-type)
  "Check if NODE-TYPE is a bracket character."
  (member node-type '("(" ")" "{" "}" "[" "]")))

(defun evilmi-tsx-treesit--match-bracket ()
  "Return matching bracket position if on a bracket."
  (cond
   ((looking-at-p "[({\\[]")
    (list (save-excursion (forward-sexp) (point)) 0))
   ((looking-back "[)}\\]]" 1)
    (list (save-excursion (backward-sexp) (point)) 0))
   (t nil)))

(defun evilmi-tsx-treesit--match-jsx-tag ()
  "Return matching JSX tag position if inside JSX."
  (when (jtsx-jsx-context-p)
    (let ((enclosing (jtsx-enclosing-jsx-element-at-point)))
      (when enclosing
        (let ((start (treesit-node-start enclosing))
              (end (treesit-node-end enclosing)))
          (if (> (point) (+ start (/ (- end start) 2)))
              (list end 1) ; closer to closing
            (list start 0)))))))

(defun evilmi-tsx-treesit-get-tag ()
  "evil-matchit get-tag function for TSX."
  (let* ((node (jtsx-treesit-node-at (point)))
         (type (treesit-node-type node)))
    (or
     ;; 1. 括号优先
     (when (evilmi-tsx-treesit--on-bracket-p type)
       (evilmi-tsx-treesit--match-bracket))
     ;; 2. JSX 标签
     (evilmi-tsx-treesit--match-jsx-tag)
     ;; 3. 无匹配
     nil)))

(defun evilmi-tsx-treesit-jump (info _num)
  "evil-matchit jump function for TSX."
  (goto-char (car info)))

(use-package! evil-matchit
  :hook (web-mode html-mode tsx-ts-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html))
  (evilmi-load-plugin-rules '(tsx-ts-mode) '(simple javascript jtsx html))
  :config
  ;; (global-evil-matchit-mode 1)
  )
(setq evilmi-debug nil)

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
(use-package! olivetti
  :config
  ;; https://www.npmjs.com/package/readability-cli
  (setq eww-retrieve-command '("readable"))
  ;; (setq eww-retrieve-command nil)
  :hook (eww-mode . olivetti-mode))

(use-package! vimrc-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode)))

;; (use-package! treesit)
;; (use-package! rust-ts-mode
;;   :mode ("\\.rs$" . rust-ts-mode))

;; (use-package! treesit-auto
;;   :config
;;   ;; (setq my/js-tsauto-config
;;   ;;       (make-treesit-auto-recipe
;;   ;;        :lang 'javascript
;;   ;;        :ts-mode 'js-ts-mode
;;   ;;        :remap '(js2-mode rjsx-mode js-mode javascript-mode)
;;   ;;        :url "https://github.com/tree-sitter/tree-sitter-javascript"
;;   ;;        :revision "master"
;;   ;;        :source-dir "src"))
;;   ;; (setq my/tsx-tsauto-config
;;   ;;       (make-treesit-auto-recipe
;;   ;;        :lang 'tsx
;;   ;;        :ts-mode 'tsx-ts-mode
;;   ;;        :remap 'typescript-tsx-mode
;;   ;;        :url "https://github.com/tree-sitter/tree-sitter-typescript"
;;   ;;        :revision "master"
;;   ;;        :source-dir "tsx/src"))
;;   ;; (add-to-list 'treesit-auto-recipe-list my/js-tsauto-config)
;;   ;; (add-to-list 'treesit-auto-recipe-list my/tsx-tsauto-config)
;;   (setq treesit-auto-langs '(json javascript typescript rust tsx yaml toml))
;;   (treesit-auto-add-to-auto-mode-alist '(rust toml json javascript typescript tsx))
;;   ;; (delete 'rust treesit-auto-langs)
;;   ;; (global-treesit-auto-mode)
;;   )

(after! gptel
  (gptel-make-openai "DeepSeek"
    :key 'gptel-api-key
    :host "ark.cn-beijing.volces.com"
    :endpoint "/api/v3/chat/completions"
    :stream t
    :models '("deepseek-v3-241226"))
  (setq gptel-model 'claude-sonnet-4)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (set-popup-rule! "^\\*Copilot" :side 'right :size 0.5 :quit nil :select t)

  (map! :leader
        :desc "Creat new chat"
        "o l l" #'my/gptel-find-chat)

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
    (funcall gptel-default-mode)
    (unless gptel-mode (gptel-mode 1)))

  (defun gptel-rename-chat (&optional _beg _end)
    "Ask the LLM to suggest a concise filename for the current gptel chat buffer."
    (interactive)
    (unless gptel-mode
      (user-error "This command is intended for gptel chat buffers."))
    (when (s-suffix? ".chat" (buffer-file-name))
      (let* ((orig-buffer (current-buffer))
             (gptel-model 'gpt-4o-mini))
        (gptel-request
            (concat
             "```" (if (eq major-mode 'org-mode) "org" "markdown") "\n"
             (buffer-substring-no-properties (point-min) (point-max))
             "\n```")
          :system (format
                   "I will provide a transcript of a chat with an LLM. \
Suggest a short and informative name for a file to store this chat in. \
Use the following guidelines:
- be very concise, one very short sentence at most
- no spaces, use underscores if required
- return ONLY the title, no explanation or summary
- append the extension .%s"
                   (if (eq major-mode 'org-mode) "org" "md"))
          :stream nil
          :callback
          (lambda (resp info)
            (if (stringp resp)
                (when (buffer-live-p orig-buffer)
                  (with-current-buffer orig-buffer
                    (rename-visited-file resp)))
              (message "Error(%s): no response." (plist-get info :status))))))))


  ;;   (setq gptel-directives
  ;;         '((default . "You are a large language model and a professional programmer.

  ;; A special requirement: at the end of the first response, always summarize the conversation into a very short title and output it surrounded by <summarized_title>.
  ;; For example, if the user asks about the usage of asyncio in python, add <summarized_title>python-asyncio-usage</summarized_title> at the end.
  ;; Only output the summarized title once. If it already present in the conversation history, don't output it again.

  ;; ")))
  ;;   (defun my/gptel-rename-buffer-from-title (beg end)
  ;;     "Parse <summarized_title>...</summarized_title> from the LLM response and rename buffer."
  ;;     (save-excursion
  ;;       (message "here?")
  ;;       (goto-char beg)
  ;;       (when (re-search-forward "<summarized_title>\\([^<]+\\)</summarized_title>" end t)
  ;;         (let ((title (match-string 1)))
  ;;           (rename-buffer (format "*gptel*<%s>" (string-trim title)) t)))))
  (add-hook 'gptel-post-response-functions #'gptel-rename-chat))

(after! forge
  (add-to-list 'forge-alist
               '("git.tigerbrokers.net"
                 "git.tigerbrokers.net/api/v4"
                 "git.tigerbrokers.net"
                 forge-gitlab-repository)))

(after! code-review
  (setq code-review-gitlab-host "git.tigerbrokers.net/api")
  (setq code-review-gitlab-graphql-host "git.tigerbrokers.net/api")
  (setq code-review-gitlab-base-url "git.tigerbrokers.net"))

(use-package! lsp-proxy
  :load-path "~/.doom.d/extensions/lsp-proxy"
  :config
  (setq lsp-proxy-log-level 0)
  (setq lsp-proxy-log-max 0)
  (setq lsp-proxy-enable-bytecode nil)
  (setq lsp-proxy-enable-hover-eldoc t)
  (setq lsp-proxy-xref-optimization-strategy 'optimized)
  ;; (setq lsp-proxy-xref-optimization-strategy 'lazy)
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
               scss-mode-hook
               lua-mode-hook
               lua-ts-mode-hook
               css-mode-hook
               ) #'lsp-proxy-mode)
  (setq lsp-proxy-inlay-hints-mode-config '(rust-mode rust-ts-mode tsx-ts-mode typescript-ts-mode))
  ;; (add-hook! '(rust-mode-hook rust-ts-mode-hook) #'lsp-proxy-inlay-hints-mode)
  (setq lsp-proxy--send-changes-idle-time 0)
  (setq lsp-proxy-diagnostics-provider :auto)
  (setq lsp-proxy-max-completion-item 30)
  (set-lookup-handlers! 'lsp-proxy-mode
    :definition '(lsp-proxy-find-definition :async t)
    :references '(lsp-proxy-find-references :async t)
    :implementations '(lsp-proxy-find-implementations :async t)
    :type-definition '(lsp-proxy-find-type-definition :async t)
    :documentation '(lsp-proxy-describe-thing-at-point :async t)))

(use-package! expreg)

;; (use-package! lsp-proxy-remote
;;   :load-path "~/.doom.d/extensions/lsp-proxy"
;;   :after lsp-proxy)

(use-package! claude-code
  :custom
  (claude-code-terminal-backend 'vterm)
  :config
  (claude-code-mode)
  (set-popup-rule! "^\\*claude" :side 'right :size 0.5 :quit nil :select t))

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

(defun open-with-zed ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "zed" nil nil nil (list (concat buffer-file-name ":" line ":" column)))))

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

(setq code-review-log-raw-request-responses nil)

(defun my-magit--normalize-log-args (args)
  "Normalize log arguments to ensure they are a flat list of strings."
  (cond
   ((null args) (list "--graph" "--decorate"))
   ((and (listp args) (listp (car args))) (car args))
   ((listp args) args)
   (t (list args))))

;;;###autoload
(defun my-magit-log-directory (directory &optional args files)
  "Show git log for all files in DIRECTORY.
With prefix argument, prompt for directory. Otherwise use current directory."
  (interactive
   (let* ((default-dir (if (and (buffer-file-name)
                                (file-directory-p default-directory))
                           default-directory
                         (magit-toplevel)))
          (dir (if current-prefix-arg
                   (read-directory-name "Directory: " default-dir)
                 default-dir)))
     (list (expand-file-name dir))))
  (let* ((rel-dir (file-relative-name directory (magit-toplevel)))
         (log-args (my-magit--normalize-log-args (or args (magit-log-arguments))))
         (path-spec (if (string= rel-dir ".") nil (list rel-dir))))
    (magit-log-setup-buffer
     nil                     ; revs (nil means current branch)
     log-args               ; args
     path-spec              ; files
     nil                    ; locked
     nil)))                 ; focus

;;;###autoload
(defun my-magit-log-current-directory ()
  "Show git log for current directory."
  (interactive)
  (my-magit-log-directory default-directory))

;;;###autoload
(defun my-magit-log-directory-since (directory since)
  "Show git log for DIRECTORY since a specific date/commit."
  (interactive
   (let ((dir (read-directory-name "Directory: "
                                   (if (buffer-file-name)
                                       (file-name-directory (buffer-file-name))
                                     default-directory))))
     (list dir
           (read-string "Since (date/commit): " "--since=1.week.ago"))))
  (let* ((rel-dir (file-relative-name directory (magit-toplevel)))
         (args (list since "--graph" "--decorate" "--oneline"))
         (path-spec (if (string= rel-dir ".") nil (list rel-dir))))
    (magit-log-setup-buffer
     nil        ; revs
     args       ; args
     path-spec  ; files
     nil        ; locked
     nil)))     ; focus

;;;###autoload
(defun my-magit-log-directory-with-stats (directory)
  "Show git log for DIRECTORY with file change statistics."
  (interactive
   (list (read-directory-name "Directory: "
                              (if (buffer-file-name)
                                  (file-name-directory (buffer-file-name))
                                default-directory))))
  (let* ((rel-dir (file-relative-name directory (magit-toplevel)))
         (args (list "--stat" "--graph" "--decorate"))
         (path-spec (if (string= rel-dir ".") nil (list rel-dir))))
    (magit-log-setup-buffer
     nil        ; revs
     args       ; args
     path-spec  ; files
     nil        ; locked
     nil)))     ; focus

;; 为 dired 模式添加便捷功能
;;;###autoload
(defun my-dired-magit-log-directory ()
  "Show git log for directory in dired."
  (interactive)
  (when (derived-mode-p 'dired-mode)
    (my-magit-log-directory (dired-current-directory))))

;; 在文件中快速查看所在目录的 git log
;;;###autoload
(defun my-magit-log-file-directory ()
  "Show git log for the directory containing current file."
  (interactive)
  (if (buffer-file-name)
      (my-magit-log-directory (file-name-directory (buffer-file-name)))
    (user-error "Current buffer is not visiting a file")))

;; Doom Emacs 配置
(after! magit
  ;; 添加键绑定
  (map! :localleader
        :map magit-mode-map
        (:prefix ("l" . "log")
                 "d" #'my-magit-log-directory
                 "D" #'my-magit-log-current-directory
                 ;; "o" #'my-magit-log-directory-with-options
                 "s" #'my-magit-log-directory-since
                 ;; "a" #'my-magit-log-directory-by-author
                 "S" #'my-magit-log-directory-with-stats
                 ;; "f" #'my-magit-log-directory-files-only
                 ))

  ;; dired 模式键绑定
  (map! :map dired-mode-map
        :localleader
        "g l" #'my-dired-magit-log-directory)

  ;; 在 magit status buffer 中添加目录 log 的快捷操作
  (transient-define-suffix my-magit-log-directory-transient (directory)
    "Log directory"
    :description "log directory"
    :key "D"
    (interactive (list (read-directory-name "Directory: " default-directory)))
    (my-magit-log-directory directory))

  ;; 将其添加到 magit-log transient 中
  (transient-append-suffix 'magit-log "l"
    '("D" "Directory log" my-magit-log-directory-transient)))

;; 增强版：显示目录下文件的详细变更历史
;;;###autoload
(defun my-magit-log-directory-detailed (directory)
  "Show detailed git log for DIRECTORY with patch information."
  (interactive
   (list (read-directory-name "Directory: "
                              (if (buffer-file-name)
                                  (file-name-directory (buffer-file-name))
                                default-directory))))
  (let* ((rel-dir (file-relative-name directory (magit-toplevel)))
         (args (list "--patch" "--graph" "--decorate" "--follow"))
         (path-spec (if (string= rel-dir ".") nil (list rel-dir))))
    (magit-log-setup-buffer
     nil        ; revs
     args       ; args
     path-spec  ; files
     nil        ; locked
     nil)))     ; focus
