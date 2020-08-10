;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
;;
;;;; font-size
(setq-default doom-font (font-spec :family "Monaco" :size 14))
;;;;;js2-mode
(after! js2-mode
  (define-key js2-mode-map (kbd "C-c j") 'js-doc-insert-function-doc)
  (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag))

;;;; js-mode
(add-hook 'js-mode-hook 'js2-minor-mode)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))
;; (setq js-expr-indent-offset 0)


;;;; web-mode
(after! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-comment-style 2
        web-mode-enable-auto-quoting nil
        web-mode-content-types-alist '(("jsx" . ".*\\.js\\'") ("vue" . ".*\\.vue\\'")))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(setq flycheck-checker-error-threshold 50)
;;让web-mode支持javascript-eslint，默认不支持
;; (after! 'flycheck
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; )
;; (lsp-ui-flycheck-enable) 这个方法会默认设置flycheck-checker为lsp-ui，当设置了这个值
;; 后，flycheck就只会使用这一个checker进行检查，否则才会遍历flycheck-checkers这个列表中的可用
;; checker依次做检查。此处禁用enable这个函数，才能同时启用lsp-ui和javascript-eslint来检查web-mode下的vue文件,
;; 缺点是不能同时起作用，只有修复了lsp-ui的warning之后，才会再使用eslint检查
;; 补充：设置了 :none 就不使用 lsp-ui 做为 checker 了， :(
(add-hook! lsp-ui-mode
  ;; (setq lsp-ui-doc-enable t
  ;;       lsp-ui-doc-use-webkit nil
  ;;       lsp-ui-doc-delay 0.2
  ;;       lsp-ui-doc-include-signature t
  ;;       lsp-ui-doc-position 'at-point
  ;;       lsp-ui-doc-border (face-foreground 'default)
  ;;       lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

  ;;       lsp-ui-sideline-enable t
  ;;       lsp-ui-sideline-show-hover nil
  ;;       lsp-ui-sideline-show-diagnostics nil
  ;;       lsp-ui-sideline-ignore-duplicate t
  ;;       ;; lsp-ui-sideline-show-code-actions nil ;; 禁用可执行提醒，如 refactor remove 等
  ;;       )
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
  ;; ;; `C-g' to close doc
  ;; (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  ;; ;; Reset `lsp-ui-doc-background' after loading theme
  ;; (add-hook 'after-load-theme-hook
  ;;           (lambda ()
  ;;             (setq lsp-ui-doc-border (face-foreground 'default))
  ;;             (set-face-background 'lsp-ui-doc-background
  ;;                                  (face-background 'tooltip))))
  ;; (setq lsp-eldoc-prefer-signature-help nil
  ;;       lsp-eldoc-enable-signature-help nil
  ;;       lsp-eldoc-enable-hover t)
  (cond ((and (equal mode-name "Web") (equal web-mode-content-type "vue")) ;; 放在lsp-ui-mode-hook 里面是因为它比web-mode 执行晚，否则 lsp-prefer-flymake 会又被覆盖
         (my/web-vue-setup))))
(defun my/web-vue-setup()
  "Setup for js related."
  (setq-local lsp-prefer-flymake :none)
  ;; (setq lsp-enable-completion-at-point t)
  ;; (setq company-backends (remove 'company-css company-backends))
  ;; (setq company-backends (remove 'company-web-html company-backends))
  ;; (setq company-backends (remove 'company-lsp company-backends))
  ;; (setq company-backends (remove 'company-yasnippet company-backends))
  )

(after! lsp-mode
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-log-io nil) ;; 开启log,每个project开启一个单独的lsp-log
  (setq lsp-print-performance nil)
  (setq lsp-eldoc-render-all nil)
  )




;;;; osx-keys
(when IS-MAC
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))

;; ivy
(map!
 (:after ivy
   (:map ivy-minibuffer-map
     "RET" #'ivy-alt-done))
 )
;; avy
(map!
 :g "M-g g" #'avy-goto-line
 :g  "M-g M-g" #'avy-goto-line
 )

;; evil-undo
(setq evil-want-fine-undo 'fine)

;; evil-matchit 只在 web-mode 和 html-mode 下开启这个 mode ，因为它在 js 等 mode 下有 bug ，使用它主要解决 doom-emacs 自带的 % 功能不支持 html 标签匹配跳转
(use-package! evil-matchit-mode
  :hook (web-mode html-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html)))

;; disable deft auto save
(setq deft-auto-save-interval 0)

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


(set-popup-rule! "^\\* \\(Chez\\|Mit\\) REPL \\*" :side 'right :quit nil :size 0.5 :select nil :modeline t)
;; (set-popup-rule! "^\\*leetcode-testcase\\*" :side 'right :quit nil :size 0.5 :select nil :modeline t)

;;;; leetcode
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

;; 让 web-mode 支持 mako 文件
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))

(setq rustic-lsp-server 'rust-analyzer)

;; emacs-rime
(use-package! rime
  :bind
  ("M-j" . #'+rime-convert-string-at-point)
  ("C-SPC" . #'toggle-input-method)
  (:map rime-active-mode-map
    ("M-j" . #'rime-inline-ascii))
  (:map rime-mode-map
    ("C-`" . #'rime-send-keybinding))
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.doom.d/librime/dist")
  (rime-inline-ascii-trigger 'shift-l)
  :config
  (defun +rime-force-enable ()
    "Forced into Chinese input state.
If current input method is not `rime', active it first. If it is
currently in the `evil' non-editable state, then switch to
`evil-insert-state'."
    (interactive)
    (let ((input-method "rime"))
      (unless (string= current-input-method input-method)
        (activate-input-method input-method))
      (when (rime-predicate-evil-mode-p)
        (if (= (+ 1 (point)) (line-end-position))
            (evil-append 1)
          (evil-insert 1)))
      (rime-force-enable)))

  (defun +rime-convert-string-at-point ()
    "Convert the string at point to Chinese using the current input scheme.
First call `+rime-force-enable' to active the input method, and
then search back from the current cursor for available string (if
a string is selected, use it) as the input code, call the current
input scheme to convert to Chinese."
    (interactive)
    (+rime-force-enable)
    (let ((string (if mark-active
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (buffer-substring-no-properties
                     (line-beginning-position) (point))))
          code
          length)
      (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
             (setq code (replace-regexp-in-string
                         "^[-']" ""
                         (match-string 0 string)))
             (setq length (length code))
             (setq code (replace-regexp-in-string " +" "" code))
             (if mark-active
                 (delete-region (region-beginning) (region-end))
               (when (> length 0)
                 (delete-char (- 0 length))))
             (when (> length 0)
               (setq unread-command-events
                     (append (listify-key-sequence code)
                             unread-command-events))))
            (t (message "`+rime-convert-string-at-point' did nothing.")))))
  (setq-default rime-disable-predicates
                '(rime-predicate-evil-mode-p
                  rime-predicate-after-alphabet-char-p
                  rime-predicate-prog-in-code-p))
  (setq-default rime-inline-predicates
                '(rime-predicate-space-after-cc-p)))

(use-package! insert-translated-name)

(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory org-brain-path)
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))

(use-package! company
  :init
  (setq company-minimum-prefix-length 1
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex))

(use-package! company-lsp
  :defer t
  :config
  (setq company-lsp-cache-candidates 'auto))

(use-package! lsp-mode
  :init
  ;; (setq lsp-prefer-capf nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-clients-typescript-log-verbosity "off")) ;;; 关闭 typescript-langauge-server 的 tsserver log 文件生成

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

(add-to-list 'company-transformers 'company//sort-by-tabnine t)


(defvar +lsp-company-backend '(company-lsp :with company-tabnine :separate))
;; (defvar +lsp-company-backend 'company-lsp)
(defvar +lsp-capf-blacklist '(ts-ls gopls))

;;;; change js2-mode's company-backend to company-lsp
(defadvice! +lsp-init-company-h-my ()
  :override #'+lsp-init-company-h
  (if (not (bound-and-true-p company-mode))
      (progn
        (remove-hook 'company-mode-hook #'+lsp-init-company-h t)
        (add-hook 'company-mode-hook #'+lsp-init-company-h-my t t))
    (let ((preferred-backend +lsp-company-backend))
      (lsp-foreach-workspace
       (when (memq (lsp--client-server-id (lsp--workspace-client lsp--cur-workspace))
                   +lsp-capf-blacklist)
         (setq preferred-backend 'company-lsp)))
      (if (eq 'company-capf preferred-backend)
          ;; use capf backend
          (progn
            (setq-local lsp-enable-completion-at-point t)
            (setq-local lsp-prefer-capf t)
            (setq-local company-backends
                        (cons preferred-backend (remq 'company-capf company-backends))))
        ;; use company-lsp backend (may need to be loaded first)
        (require 'company-lsp)
        (setq-local lsp-enable-completion-at-point nil)
        (setq-local lsp-prefer-capf nil)
        (setq-local company-backends
                    (cons preferred-backend (remq 'company-capf company-backends)))
        (setq-default company-lsp-cache-candidates 'auto))
      (remove-hook 'company-mode-hook #'+lsp-init-company-h-my t))))

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

(map! "M-p" #'switch-to-prev-buffer
      "M-n" #'switch-to-next-buffer)

(use-package! eslintd-fix
  :commands eslintd-fix
  :config
  (setq-hook! 'eslintd-fix-mode-hook
    flycheck-javascript-eslint-executable eslintd-fix-executable))
