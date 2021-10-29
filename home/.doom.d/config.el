;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq user-full-name "JadeStrong"
      user-mail-address "jadestrong@163.com"

      doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-one

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      ;; display-line-numbers-type nil

      ;; lsp
      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable t
      lsp-ui-sideline-show-code-actions nil
      lsp-enable-symbol-highlighting nil
      lsp-ui-doc-enable nil
      lsp-auto-guess-root t
      read-process-output-max (* 1024 1024)
      lsp-eldoc-render-all nil
      lsp-clients-typescript-log-verbosity "off"
      ;; +lsp-company-backends '(company-tabnine company-capf :with company-yasnippet)
      ;; +lsp-company-backends '(company-capf :with company-tabnine :separate)
      ;; +lsp-company-backends '(company-capf company-yasnippet :with company-tabnine :separate)
      +lsp-company-backends '(:separate company-tabnine-capf company-yasnippet)
      lsp-eslint-enable nil
      ;; lsp-eslint-server-command `("node" "/Users/jadestrong/.vscode/extensions/dbaeumer.vscode-eslint-2.1.8/server/out/eslintServer.js" "--stdio")
      lsp-vetur-experimental-template-interpolation-service nil
      +lsp-prompt-to-install-server 'quiet
      lsp-enable-indentation nil ;; don't use lsp-format-region as indent-region-function

      ;; disable deft auto save
      deft-auto-save-interval 0

      ;; company and company-lsp
      company-minimum-prefix-length 1

      ;; rust
      rustic-lsp-server 'rust-analyzer
      rustic-analyzer-command '("/Users/jadestrong/Library/Application\ Support/Code/User/globalStorage/matklad.rust-analyzer/rust-analyzer-x86_64-apple-darwin")
      lsp-rust-analyzer-cargo-load-out-dirs-from-check t ;; support extern C suggest
      lsp-rust-analyzer-proc-macro-enable t ;; same above

      lsp-rust-analyzer-experimental-proc-attr-macros t ;; 内嵌变量提示
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints t
      lsp-rust-analyzer-max-inlay-hint-length 25

      ;; More common use-case
      evil-ex-substitute-global t
      evil-want-fine-undo t
      ;; evil-want-minibuffer t
      ;; evil-collection-setup-minibuffer t
      auto-save-default nil
      ;; flycheck-checker-error-threshold nil
      flycheck-highlighting-style `(conditional 10 level-face (delimiters "" ""))
      +company-backend-alist '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
                               (prog-mode company-tabnine-capf company-yasnippet)
                               (conf-mode company-capf company-dabbrev-code company-yasnippet))
      )

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))
(setq lsp-log-io nil)

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
      (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line)))
       :i "M-j" #'+rime-convert-string-at-point)
      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles
      "p t" #'doom/ivy-tasks)

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

(add-hook 'js-mode-hook 'js2-minor-mode)
(advice-add 'js--multi-line-declaration-indentation :around (lambda (orig-fun &rest args) nil))

;;; :tools magit
(setq magit-inhibit-save-previous-winconf t
      ;; transient-values '((magit-rebase "--autosquash"))
      )


;;; :complete company
;; (use-package! company-tabnine
;;   :when (featurep! :completion company)
;;   :config
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-1
;;             candidates-2)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-2))
;;             (push candidate candidates-1)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-1 (nreverse candidates-1))
;;         (setq candidates-2 (nreverse candidates-2))
;;         (nconc (seq-take candidates-1 2)
;;                (seq-take candidates-2 2)
;;                (seq-drop candidates-1 2)
;;                (seq-drop candidates-2 2)))))
;;   (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;   ;; The free version of TabNine is good enough,
;;   ;; and below code is recommended that TabNine not always
;;   ;; prompt me to purchase a paid version in a large project.
;;   ;; 禁止tabnine提示升级付费版本
;;   (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
;;       (let ((company-message-func (ad-get-arg 0)))
;;         (when (and company-message-func
;;                    (stringp (funcall company-message-func)))
;;           (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
;;             ad-do-it))))
;;   ;;将tabnine添加到backends
;;   (add-to-list 'company-backends 'company-tabnine))



;; (defun company/remove-duplicate-cands (candidates)
;;   ;; (dolist (candidate candidates)
;;   ;;   (setq backend-property (get-text-property 0 'company-backend candidate))
;;   ;;   (setq completion-item (get-text-property 0 'lsp-completion-item candidate))
;;   ;;   (message "%s" backend-property)
;;   ;;   (message "%s" completion-item))
;;   ;; candidates
;;   (let ((newseq))
;;     (dolist (candidate candidates)
;;       (setq backend-property (get-text-property 0 'company-backend candidate))
;;       (setq completion-item (get-text-property 0 'lsp-completion-item candidate))
;;       (if (not (member candidate newseq))
;;           (push candidate newseq)
;;         (if completion-item
;;             (progn
;;               (setq newseq (delq candidate candidates))
;;               (push candidate candidates)
;;               )
;;           )
;;         )
;;       )
;;     (nreverse newseq))
;;   )

;;; :lang web
;; 让 web-mode 支持 mako 文件
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
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

;; disable org-mode company-mode
(defun disable-company-hook ()
  (company-mode -1))
(add-hook! (org-mode markdown-mode text-mode) 'disable-company-hook)

;; (setq flycheck-checker-error-threshold 50)

;; 同时支持 lsp 和 javascript-eslint
(defun creature/lsp-eslint-checker-init ()
  (when (and flycheck-mode
             (flycheck-valid-checker-p 'lsp)
             (flycheck-valid-checker-p 'javascript-eslint))
    (make-local-variable 'flycheck-checkers)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)))
(with-eval-after-load 'lsp-diagnostics
  (add-hook! lsp-diagnostics-mode #'creature/lsp-eslint-checker-init))

;; Optional: ensure flycheck cycles, both when going backward and forward.
;; Tries to handle arguments correctly.
;; Since flycheck-previous-error is written in terms of flycheck-next-error,
;; advising the latter is enough.
(defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
  (message "flycheck-next-error called with args %S %S" n reset)
  (condition-case err
      (apply orig-fun (list n reset))
    ((user-error)
     (let ((error-count (length flycheck-current-errors)))
       (if (and
            (> error-count 0)                   ; There are errors so we can cycle.
            (equal (error-message-string err) "No more Flycheck errors"))
           ;; We need to cycle.
           (let* ((req-n (if (numberp n) n 1)) ; Requested displacement.
                  ; An universal argument is taken as reset, so shouldn't fail.
                  (curr-pos (if (> req-n 0) (- error-count 1) 0)) ; 0-indexed.
                  (next-pos (mod (+ curr-pos req-n) error-count))) ; next-pos must be 1-indexed
             (message "error-count %S; req-n %S; curr-pos %S; next-pos %S" error-count req-n curr-pos next-pos)
             ; orig-fun is flycheck-next-error (but without advise)
             ; Argument to flycheck-next-error must be 1-based.
             (apply orig-fun (list (+ 1 next-pos) 'reset)))
         (signal (car err) (cdr err)))))))

(advice-add 'flycheck-next-error :around #'flycheck-next-error-loop-advice)

;;; leetcode
(use-package! leetcode
  :init
  (setq leetcode-prefer-language "javascript")
  (setq leetcode-prefer-sql "mysql")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/.leetcode")
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

(use-package! doom-todo-ivy)
(setq magit-todos-nice nil)

;; (use-package! prescient
;;   :hook (company-mode . company-prescient-mode)
;;   :hook (company-mode . prescient-persist-mode)
;;   :config
;;   (setq prescient-save-file (concat doom-cache-dir "prescient-save.el")))

;; (use-package! citre
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

;;; Language customizations

;; evil-matchit 只在 web-mode 和 html-mode 下开启这个 mode ，因为它在 js 等 mode 下有 bug
;; 使用它主要解决 doom-emacs 自带的 % 功能不支持 html 标签匹配跳转
(use-package! evil-matchit-mode
  :hook (web-mode html-mode)
  :init
  (evilmi-load-plugin-rules '(web-mode) '(simple template html))
  (evilmi-load-plugin-rules '(html-mode) '(simple template html)))

;；遇到大文件时语法检查贼慢，因此强制使用 fundamental-mode
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

;; Disable it only for rust buffers - doc not auto display in mini buffer
(setq-hook! 'rustic-mode-hook lsp-signature-auto-activate nil)

;;
;;; Fix bugs

;; Fix a bug which will throw a file not exist error when create a new file
(defadvice! +lsp-clients-flow-activate-p (file-name _mode)
  :override #'lsp-clients-flow-activate-p
  (and (derived-mode-p 'js-mode 'web-mode 'js2-mode 'flow-js2-mode 'rjsx-mode)
       (not (derived-mode-p 'json-mode))
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

(defadvice! +plantuml-server-encode-url (string)
  :override #'plantuml-server-encode-url
  (let* ((coding-system (or buffer-file-coding-system
                            "utf8"))
         (encoded-string (base64-encode-string (encode-coding-string string 'utf-8) t)))
    (concat plantuml-server-url "/" plantuml-output-type "/-base64-" encoded-string)))

;; See https://github.com/universal-ctags/citre/wiki/Use-Citre-together-with-lsp-mode
(define-advice xref--create-fetcher (:around (-fn &rest -args) fallback)
  (let ((fetcher (apply -fn -args))
        (citre-fetcher
         (let ((xref-backend-functions '(citre-xref-backend t)))
           (apply -fn -args))))
    (lambda ()
      (or (with-demoted-errors "%s, fallback to citre"
            (funcall fetcher))
          (funcall citre-fetcher)))))

;; 修复当安装了 git hooks 插件后， magit-process-mode 中输出的内容有颜色时导致的乱码问题
(defun color-buffer (proc &rest args)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))
(advice-add 'magit-process-filter :after #'color-buffer)

;; 修复 rjsx-mode 反注释会影响行内的 // 的 bug
(setq rjsx-comment-start-skip "[[:space:]]*\\(?://+\\|{?/\\*+\\)")
(defun +comment-search-forward (limit &optional noerror)
  "Find a comment start between point and LIMIT.
Moves point to inside the comment and returns the position of the
comment-starter.  If no comment is found, moves point to LIMIT
and raises an error or returns nil if NOERROR is non-nil.

Ensure that `comment-normalize-vars' has been called before you use this."
  (if (not comment-use-syntax)
      (if (re-search-forward rjsx-comment-start-skip limit noerror)
	  (or (match-end 1) (match-beginning 0))
	(goto-char limit)
	(unless noerror (error "No comment")))
    (let* ((pt (point))
	   ;; Assume (at first) that pt is outside of any string.
	   (s (parse-partial-sexp pt (or limit (point-max)) nil nil
				  (if comment-use-global-state (syntax-ppss pt))
				  t)))
      (when (and (nth 8 s) (nth 3 s) (not comment-use-global-state))
	;; The search ended at eol inside a string.  Try to see if it
	;; works better when we assume that pt is inside a string.
	(setq s (parse-partial-sexp
		 pt (or limit (point-max)) nil nil
		 (list nil nil nil (nth 3 s) nil nil nil nil)
		 t)))
      (if (or (not (and (nth 8 s) (not (nth 3 s))))
	      ;; Make sure the comment starts after PT.
	      (< (nth 8 s) pt))
	  (unless noerror (error "No comment"))
	;; We found the comment.
	(let ((pos (point))
	      (start (nth 8 s))
	      (bol (line-beginning-position))
	      (end nil))
	  (while (and (null end) (>= (point) bol))
	    (if (looking-at rjsx-comment-start-skip)
		(setq end (min (or limit (point-max)) (match-end 0)))
	      (backward-char)))
	  (goto-char (or end pos))
	  start)))))

(defun +comment-forward (&optional n)
  "Skip forward over N comments.
Just like `forward-comment` but only for positive N and can use regexps instead of syntax."
  (setq n (or n 1))
  (if (< n 0) (error "No comment-backward")
    (if comment-use-syntax (forward-comment n)
      (while (> n 0)
        (setq n
              (if (or (forward-comment 1)
                      (and (looking-at rjsx-comment-start-skip)
                           (goto-char (match-end 0))
                           (re-search-forward comment-end-skip nil 'move)))
                  (1- n) -1)))
      (= n 0))))

(defadvice! +rjsx-uncomment-region-function (beg end &optional _)
  :override #'rjsx-uncomment-region-function
  (js2-mode-wait-for-parse
   (lambda ()
     (goto-char beg)
     (setq end (copy-marker end))
     (let (cs ts te ce matched-start)
       ;; find comment start
       (while (and (<= (point) end)
                   (setq ipt (point))
                   (setq spt (+comment-search-forward end t)))
         (let ((ept (progn
                      (goto-char spt)
                      (unless (or (+comment-forward)
                                  (eobp))
                        (error "Can't find the comment end"))
                      (point))))
           (save-restriction
             (narrow-to-region spt ept)
             ;; delete comment-start
             (goto-char ipt)
             (setq matched-start
                   (and (re-search-forward comment-start-skip end t 1)
                        (match-string-no-properties 0)))
             (setq cs (match-beginning 1))
             (setq ts (match-end 1))
             (goto-char cs)
             (delete-region cs ts)

             ;; delete comment-padding start
             (when (and comment-padding (looking-at (regexp-quote comment-padding)))
               (delete-region (point) (+ (point) (length comment-padding))))

             ;; find comment end
             (when (re-search-forward (if (string-match "//+" matched-start) "\n" "\\*/}?") end t 1)
               (setq te (or (match-beginning 1) (match-beginning 0)))
               (setq ce (or (match-end 1) (match-end 0)))
               (goto-char te)

               ;; delete commend-end if it's not a newline
               (unless (string= "\n" (match-string-no-properties 0))
                 (delete-region te ce)

                 ;; delete comment-padding end
                 (when comment-padding
                   (backward-char (length comment-padding))
                   (when (looking-at (regexp-quote comment-padding))
                     (delete-region (point) (+ (point) (length comment-padding))))))

               ;; unescape inner comments if any
               (save-restriction
                 (narrow-to-region cs (point))
                 (comment-quote-nested "{/*" "*/}" t)))
             (goto-char (point-max))))

         ))

     (rjsx-maybe-unwrap-expr beg end)

     (set-marker end nil))))

;; emacs-rime 在 web-mode 下判断是否在字符串内有问题，在 script 内的字符串后面输入字符时也会被识别为字符串
;; 这里通过 inside-string-p
;; https://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
(defun inside-string-p (&optional pos)
  "Test if character at POS is string.  If POS is nil, character at `(point)' is tested"
  (interactive)
  (unless pos (setq pos (point)))
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (or (member 'font-lock-string-face fontfaces)
        (member 'web-mode-javascript-string-face fontfaces))))

(defadvice! +rime-predicate-prog-in-code-p ()
  :override #'rime-predicate-prog-in-code-p
  "If cursor is in code.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (derived-mode-p 'prog-mode 'conf-mode)
       (not (or (and (nth 3 (syntax-ppss)) (inside-string-p))
                (nth 4 (syntax-ppss))))))

;; 支持拼音搜索中文文件
(use-package! pinyinlib)
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(after! orderless
 (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
;; 默认 lsp 会记住所有之前打开的 vue 项目，并每次启动的时候都会在每个项目里面都启用一个 vls 服务，这里强制其遗忘
(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; 当打开压缩后的 js 文件时， dtrt-indent-mode 会因为单行内容太长，生成的正则太长而报错，这里忽略其抛出的错误
(defadvice! +doom-detect-indentation-h nil
  :override #'doom-detect-indentation-h
  (if
      (or
       (not after-init-time)
       doom-inhibit-indent-detection doom-large-file-p
       (memq major-mode doom-detect-indentation-excluded-modes)
       (member
        (substring
         (buffer-name)
         0 1)
        '(" " "*")))
      nil
    (let
        ((inhibit-message
          (not doom-debug-p)))
      (ignore-errors
        (dtrt-indent-mode 1)))))

;;; Customize function

(defconst target-dir-path "~/Downloads/" "My Download directory")
(defun goto-download-dir ()
  (interactive)
  (dired target-dir-path))

(defun flycheck-disable-on-temp-buffers ()
  (unless (and buffer-file-name (file-exists-p buffer-file-name)) (flycheck-mode -1)))
(add-hook 'prog-mode-hook 'flycheck-disable-on-temp-buffers)

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
