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
      lsp-eldoc-render-all nil
      lsp-clients-typescript-log-verbosity "off"
      +lsp-company-backends '(company-capf :with company-tabnine :separate)
      lsp-eslint-enable nil
      ;; lsp-eslint-server-command `("node" "/Users/jadestrong/.vscode/extensions/dbaeumer.vscode-eslint-2.1.8/server/out/eslintServer.js" "--stdio")
      lsp-vetur-experimental-template-interpolation-service nil


      ;; company and company-lsp
      company-minimum-prefix-length 1
      company-lsp-match-candidate-predicate #'company-lsp-match-candidate-flex
      company-lsp-cache-candidates 'auto

      ;; rust
      rustic-lsp-server 'rust-analyzer

      ;; More common use-case
      evil-ex-substitute-global t
      evil-want-fine-undo nil
      ;; evil-want-minibuffer t
      ;; evil-collection-setup-minibuffer t
      auto-save-default nil
      ;; flycheck-checker-error-threshold nil
      )

(when IS-MAC
  (setq mac-command-modifier 'meta
        mac-option-modifier 'none))
;; (setq lsp-log-io t)

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
             (seq-drop candidates-2 2))))
  )
;; (after! company
;;   (add-to-list 'company-transformers 'company//sort-by-tabnine t))


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

;; disable org-mode company-mode
(defun disable-company-hook ()
  (company-mode -1))
(add-hook! (org-mode markdown-mode text-mode) 'disable-company-hook)



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

(use-package! prescient
  :hook (company-mode . company-prescient-mode)
  :hook (company-mode . prescient-persist-mode)
  :config
  (setq prescient-save-file (concat doom-cache-dir "prescient-save.el")))

(use-package! citre
  :defer t
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  :config
  (setq
   citre-project-root-function #'projectile-project-root))


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
;; (after! org
;;   (appendq! +ligatures-extra-symbols
;;             `(:checkbox      "☐"
;;               :pending       "◼"
;;               :checkedbox    "☑"
;;               :list_property "∷"

;;               :ellipses      "…"
;;               :arrow_right   "→"
;;               :arrow_left    "←"
;;               :title         "❤"
;;               :subtitle      "𝙩"
;;               :author        "✍"
;;               :date          "⚓"
;;               :property      "☸"
;;               :options       "⌥"
;;               :latex_class   "🄲"
;;               :latex_header  "⇥"
;;               :beamer_header "↠"
;;               :attr_latex    "🄛"
;;               :attr_html     "🄗"
;;               :begin_quote   "❮"
;;               :end_quote     "❯"
;;               :caption       "☰"
;;               :header        "›"
;;               :results       "🍌"
;;               :begin_export  "⏩"
;;               :end_export    "⏪"
;;               :properties    "⚙"
;;               :end           "∎"
;;               :priority_a   ,(propertize "🅰" 'face 'all-the-icons-red)
;;               :priority_b   ,(propertize "🅱" 'face 'all-the-icons-orange)
;;               :priority_c   ,(propertize "🅲" 'face 'all-the-icons-yellow)
;;               :priority_d   ,(propertize "🅳" 'face 'all-the-icons-green)
;;               :priority_e   ,(propertize "🅴" 'face 'all-the-icons-blue)))
;;   (set-ligatures! 'org-mode
;;                   :merge t
;;                   :checkbox      "[ ]"
;;                   :pending       "[-]"
;;                   :checkedbox    "[X]"
;;                   :list_property "::"
;;                   :em_dash       "---"
;;                   :ellipsis      "..."
;;                   :arrow_right   "->"
;;                   :arrow_left    "<-"
;;                   :title         "#+title:"
;;                   :subtitle      "#+subtitle:"
;;                   :author        "#+author:"
;;                   :date          "#+date:"
;;                   :property      "#+property:"
;;                   :options       "#+options:"
;;                   :latex_class   "#+latex_class:"
;;                   :latex_header  "#+latex_header:"
;;                   :beamer_header "#+beamer_header:"
;;                   :attr_latex    "#+attr_latex:"
;;                   :attr_html     "#+attr_latex:"
;;                   :begin_quote   "#+begin_quote"
;;                   :end_quote     "#+end_quote"
;;                   :caption       "#+caption:"
;;                   :header        "#+header:"
;;                   :begin_export  "#+begin_export"
;;                   :end_export    "#+end_export"
;;                   :results       "#+RESULTS:"
;;                   :property      ":PROPERTIES:"
;;                   :end           ":END:"
;;                   :priority_a    "[#A]"
;;                   :priority_b    "[#B]"
;;                   :priority_c    "[#C]"
;;                   :priority_d    "[#D]"
;;                   :priority_e    "[#E]")
;;   (plist-put +ligatures-extra-symbols :name "⁍"))


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

(defadvice! +rjsx-uncomment-region-function (beg end &optional _)
  :override #'rjsx-uncomment-region-function
  (js2-mode-wait-for-parse
   (lambda ()
     (goto-char beg)
     (setq end (copy-marker end))
     (let (cs ts te ce matched-start)
       ;; find comment start
       (while (and (<= (point) end)
                   (setq matched-start
                         (and (re-search-forward comment-start-skip end t 1)
                              (match-string-no-properties 0))))
         (let ((spt (match-beginning 1))
               (ept (progn
                      (goto-char spt)
                      (unless (or (forward-comment 1)
                                  (eobp))
                        (error "Can't find the comment end"))
                      (point))))
           (save-restriction
             (narrow-to-region spt ept)
             (message "spt: %s , ept: %s " spt ept)))
         ;; delete comment-start
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
             (comment-quote-nested "{/*" "*/}" t)))))

     (rjsx-maybe-unwrap-expr beg end)

     (set-marker end nil))))


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

;; 支持拼音搜索中文文件
(use-package! pinyinlib)
(defun completion--regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(after! orderless
 (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))


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

;; (use-package! tree-sitter-langs)
