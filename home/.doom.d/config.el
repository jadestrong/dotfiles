;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "JadeStrong"
      user-mail-address "jadestrong@163.com"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-one

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      ;; display-line-numbers-type nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil

      ;; disable deft auto save
      deft-auto-save-interval 0

      ;; lsp
      lsp-auto-guess-root t
      read-process-output-max (* 1024 1024)
      lsp-log-io nil
      lsp-eldoc-render-all nil
      lsp-clients-typescript-log-verbosity "off"
      +lsp-company-backends '(company-capf :with company-tabnine :separate)
      lsp-eslint-server-command `("node" "/Users/jadestrong/.vscode/extensions/dbaeumer.vscode-eslint-2.1.8/server/out/eslintServer.js" "--stdio")


      ;; company and company-lsp
      company-minimum-prefix-length 1
      company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex
      company-lsp-cache-candidates 'auto
      ;; +lsp-company-backends 'company-tabnine
      ;; +lsp-company-backends '(company-capf :with company-tabnine :separate)

      ;; rust
      rustic-lsp-server 'rust-analyzer

      ;; More common use-case
      evil-ex-substitute-global t
      evil-want-fine-undo nil
      ;; evil-want-minibuffer t
      ;; evil-collection-setup-minibuffer t
      )

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))
(setq lsp-log-io nil)

;;
;;; UI


;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "Monaco" :size 16)
      ;; doom-variable-pitch-font (font-spec :family "sans" :size 14)
      )

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

      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles)

(map!
 :g "M-g g" #'avy-goto-line
 :g  "M-g M-g" #'avy-goto-line)


(map! "M-p" #'switch-to-prev-buffer
      "M-n" #'switch-to-next-buffer)


;;
;;; Modules

(after! ivy
  ;; I prefer search matching to be ordered; it's more precise
  (add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus)))
(map! (:after ivy (:map ivy-minibuffer-map "RET" #'ivy-alt-done)))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :lang javascript
(map! :map (js2-mode-map typescript-mode-map)
      "C-c j" 'js-doc-insert-function-doc
      "@" 'js-doc-insert-tag)
;; (after! js2
;;   (define-key js2-mode-map (kbd "C-c j") 'js-doc-insert-function-doc)
;;   (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag))

(add-hook 'js-mode-hook 'js2-minor-mode)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))

;;; :tools lsp
(after! lsp-mode
  (setq lsp-auto-guess-root t
        read-process-output-max (* 1024 1024)
        lsp-log-io nil
        lsp-eldoc-render-all nil))

;;; :tools magit
(setq magit-inhibit-save-previous-winconf t
      ;; transient-values '((magit-rebase "--autosquash"))
      )


;;; :complete company
(defun company//sort-by-tabnine (candidates)
  (if (or (functionp company-backend)
          (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
      candidates
    (let ((candidates-table (make-hash-table :test #'equal))
          candidates-1
          candidates-2)
      (dolist (candidate candidates)
        (if (eq (get-text-property 0 'company-backend candidate)
                'company-tabnine)
            (unless (gethash candidate candidates-table)
              (push candidate candidates-2))
          (push candidate candidates-1)
          (puthash candidate t candidates-table)))
      (setq candidates-1 (nreverse candidates-1))
      (setq candidates-2 (nreverse candidates-2))
      (nconc (seq-take candidates-1 2)
             (seq-take candidates-2 2)
             (seq-drop candidates-1 2)
             (seq-drop candidates-2 2)))))
(after! company
  (add-to-list 'company-transformers 'company//sort-by-tabnine t))



;;; :lang web
;; 让 web-mode 支持 mako 文件
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
(defun my-web-mode-hook ()
  (setq web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-comment-style 2
        ;; web-mode-code-indent-offset 4
        ;; web-mode-css-indent-offset 4
        ;; web-mode-markup-indent-offset 2
        )
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  )
(add-hook! 'web-mode-hook #'my-web-mode-hook)

;; (after! web
;;   (setq web-mode-style-padding 0
;;         web-mode-script-padding 0
;;         web-mode-comment-style 2
;;         web-mode-code-indent-offset 4
;;         web-mode-css-indent-offset 4
;;         web-mode-markup-indent-offset 2
;;         web-mode-content-types-alist '(("jsx" . ".*\\.js\\'") ("vue" . ".*\\.vue\\'"))))


;; (after! web-mode
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-code-indent-offset 4
;;         web-mode-css-indent-offset 4
;;         web-mode-style-padding 0
;;         web-mode-script-padding 0
;;         web-mode-comment-style 2
;;         web-mode-enable-auto-quoting nil
;;         web-mode-content-types-alist '(("jsx" . ".*\\.js\\'") ("vue" . ".*\\.vue\\'")))
;;   (flycheck-add-mode 'javascript-eslint 'web-mode))

;; (setq flycheck-checker-error-threshold 50)
;;让web-mode支持javascript-eslint，默认不支持
;; (after! 'flycheck
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; )
;; (lsp-ui-flycheck-enable) 这个方法会默认设置flycheck-checker为lsp-ui，当设置了这个值
;; 后，flycheck就只会使用这一个checker进行检查，否则才会遍历flycheck-checkers这个列表中的可用
;; checker依次做检查。此处禁用enable这个函数，才能同时启用lsp-ui和javascript-eslint来检查web-mode下的vue文件,
;; 缺点是不能同时起作用，只有修复了lsp-ui的warning之后，才会再使用eslint检查
;; 补充：设置了 :none 就不使用 lsp-ui 做为 checker 了， :(
;; (add-hook! lsp-ui-mode
;;   (cond ((and (equal mode-name "Web") (equal web-mode-content-type "vue")) ;; 放在lsp-ui-mode-hook 里面是因为它比web-mode 执行晚，否则 lsp-prefer-flymake 会又被覆盖
;;          (my/web-vue-setup))))


;; (defun my/web-vue-setup()
;;   "Setup for js related."
;;   (setq-local lsp-prefer-flymake :none))

;;; leetcode
(use-package! leetcode
  :init
  (setq leetcode-prefer-language "javascript")
  (setq leetcode-prefer-sql "mysql")
  (set-popup-rule! "^\\*html\\*" :side 'right :quit nil :size 0.5 :select nil :modeline t) ;; leetcode--display-description
  :config
  (map! :map leetcode--problems-mode-map
        :localleader
        "/" #'leetcode-reset-filter
        "s" #'leetcode-set-filter-regex
        "t" #'leetcode-set-filter-tag
        "r" #'leetcode-refresh
        "g" #'leetcode-refresh-fetch))

(use-package! eslintd-fix
  :commands eslintd-fix
  :config
  (setq-hook! 'eslintd-fix-mode-hook
    flycheck-javascript-eslint-executable eslintd-fix-executable))

(use-package! insert-translated-name)


;;; :lang org
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      ;; org-roam-directory (concat org-directory "notes/")
      deft-directory (concat org-directory "deft/")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " ▼ "
      ;; org-superstar-headline-bullets-list '("#")
      )


;; (use-package! company-tabnine :ensure t)

;;
;;; Language customizations

;; evil-matchit 只在 web-mode 和 html-mode 下开启这个 mode ，因为它在 js 等 mode 下有 bug ，使用它主要解决 doom-emacs 自带的 % 功能不支持 html 标签匹配跳转
(use-package! evil-matchit-mode
  :hook (web-mode html-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html)))

;；遇到大文件时语法检查贼满，因此强制使用 fundamental-mode
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 2 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    ;; (lsp-mode -1)
                                        ; (message "Buffer is set to read-only because it is large.  Undo also disabled.")
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)


;;
;;; Fix bugs

;; Fix a bug which will throw a file not exist error when create a new file
(defadvice! +lsp-clients-flow-activate-p (file-name _mode)
  :override #'lsp-clients-flow-activate-p
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (or (lsp-clients-flow-project-p file-name)
           (and (f-file-p file-name)
                (lsp-clients-flow-tag-file-present-p file-name)))))

;; 修正 web-mode 的 stylus 不能自动缩进的问题，方案不完美，未涵盖所有情况，或许需要参考 sass-mode 或者 pug-mode 的方式，允许 tab 可以随意缩进 TODO
(defadvice! +web-mode-stylus-indentation (pos initial-column language-offset language &optional limit)
  :override #'web-mode-stylus-indentation
  (unless limit (setq limit nil))
  (let (offset h prev-line prev-indentation open-ctx)
    (setq h (web-mode-previous-line pos limit))
    (when h
      (setq prev-line (car h))
      (setq prev-indentation (cdr h))
      (cond
       ((or (string-match-p "^\\([\s\/]?\\)+[\.#&@\[:].+[^,]$" prev-line)
            (string-match-p "\s&\\(.*\\)[^,]$|&$" prev-line)
            (string-match-p "^\\(\s?\\)+\d{1,3}%" prev-line)
            (string-match-p "^\\(\s?\\)+for.+in.+$" prev-line))
        (setq offset (+ prev-indentation language-offset)))
       (t
        (setq offset prev-indentation))))
    (cons (if (<= offset initial-column) initial-column offset) nil)))

;; 修复当安装了 git hooks 插件后， magit-process-mode 中输出的内容有颜色时导致的乱码问题
(defun color-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))
(advice-add 'magit-process-filter :after #'color-buffer)


;;; Customize function

(defconst target-dir-path "~/Downloads/" "My Download directory")
(defun goto-download-dir ()
  (interactive)
  (dired target-dir-path))

(use-package! tree-sitter-langs)

;; (use-package! company
;;    :init
;;    (setq company-minimum-prefix-length 1
;;          company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex))

;; (use-package! company-lsp
;;    :defer t
;;    :config
;;    (setq company-lsp-cache-candidates 'auto))

;; workaround for company-transformers
;; (setq company-tabnine--disable-next-transform nil)
;; (defun my-company--transform-candidates (func &rest args)
;;   (if (not company-tabnine--disable-next-transform)
;;       (apply func args)
;;     (setq company-tabnine--disable-next-transform nil)
;;     (car args)))

;; (defun my-company-tabnine (func &rest args)
;;   (when (eq (car args) 'candidates)
;;     (setq company-tabnine--disable-next-transform t))
;;   (apply func args))

;; (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
;; (advice-add #'company-tabnine :around #'my-company-tabnine)


;; (defvar +lsp-company-backend '(company-lsp :with company-tabnine :separate))
;; ;; (defvar +lsp-company-backend 'company-lsp)
;; (defvar +lsp-capf-blacklist '(ts-ls gopls))

;; ;;;; change js2-mode's company-backend to company-lsp
;; (defadvice! +lsp-init-company-h-my ()
;;   :override #'+lsp-init-company-h
;;   (if (not (bound-and-true-p company-mode))
;;       (progn
;;         (remove-hook 'company-mode-hook #'+lsp-init-company-h t)
;;         (add-hook 'company-mode-hook #'+lsp-init-company-h-my t t))
;;     (let ((preferred-backend +lsp-company-backend))
;;       (lsp-foreach-workspace
;;        (when (memq (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))
;;                    +lsp-capf-blacklist)
;;          (setq preferred-backend 'company-lsp)))
;;       (if (eq 'company-capf preferred-backend)
;;           ;; use capf backend
;;           (progn
;;             (setq-local lsp-enable-completion-at-point t)
;;             (setq-local lsp-prefer-capf t)
;;             (setq-local company-backends
;;                         (cons preferred-backend (remq 'company-capf company-backends))))
;;         ;; use company-lsp backend (may need to be loaded first)
;;         (require 'company-lsp)
;;         (setq-local lsp-enable-completion-at-point nil)
;;         (setq-local lsp-prefer-capf nil)
;;         (setq-local company-backends
;;                     (cons preferred-backend (remq 'company-capf company-backends)))
;;         (setq-default company-lsp-cache-candidates 'auto))
;;       (remove-hook 'company-mode-hook #'+lsp-init-company-h-my t))))
