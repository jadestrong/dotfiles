;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

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
      ;; +lsp-company-backends '(company-tabnine company-capf :with company-yasnippet)
      ;; +lsp-company-backends '(company-capf :with company-tabnine :separate)
      ;; +lsp-company-backends '(company-capf company-yasnippet :with company-tabnine :separate)
      +lsp-company-backends '(:separate company-tabnine-capf)
      lsp-eslint-enable t
      lsp-eslint-download-url "https://github.com/jadestrong/lsp-server-binaries/blob/master/dbaeumer.vscode-eslint-2.2.2.vsix?raw=true"
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
      rustic-analyzer-command '("~/.vscode/extensions/matklad.rust-analyzer-0.3.995/server/rust-analyzer")
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
      +company-backend-alist '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
                               (prog-mode company-tabnine-capf company-yasnippet) ;; 指定 prog-mode 使用 company-tabnine-capf ，使用 rust-analyzer 服务时这个通过 +lsp-company-backend 指定的后端 revert buffer 后总是会被这个配置的值覆盖
                               (conf-mode company-capf company-dabbrev-code company-yasnippet))
      )

;;; :lang org
(setq org-directory "~/org/"
      org-archive-location (concat org-directory ".archive/%s::")
      org-roam-directory (concat org-directory "roam")
      ;; org-roam-directory "~/.roam"
      deft-directory (concat org-directory "deft/")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " ▼ "
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

;; emacs-29
(when (version= emacs-version "29.0.50")
  (general-auto-unbind-keys :off)
  (remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line))

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

;; (evil-set-initial-state 'vterm-mode 'emacs)

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
;; remove company-ispell so can disable this
;; (defun disable-company-hook ()
;;   (company-mode -1))
;; (when (featurep! :completion company)
;;   (add-hook! (org-mode markdown-mode text-mode) 'disable-company-hook))


;; (setq flycheck-checker-error-threshold 50)

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

;; Optional: ensure flycheck cycles, both when going backward and forward.
;; Tries to handle arguments correctly.
;; Since flycheck-previous-error is written in terms of flycheck-next-error,
;; advising the latter is enough.
(defun flycheck-next-error-loop-advice (orig-fun &optional n reset)
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
  (setq leetcode-prefer-language "typescript")
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

(use-package! lsp-volar)

(use-package! org-modern
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode))

(use-package! dirvish
  :config
  ;; (dirvish-override-dired-mode 1)
  (setq dirvish-depth 0)
  (setq dirvish-preview-dispatchers (remove 'directory-exa dirvish-preview-dispatchers))
  (setq! dirvish-attributes '(all-the-icons file-size))
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

;;; trigger textDocument/codeAction only when there had an error on current pointer.
(defadvice! +lsp-modeline--check-mode-actions (&rest _)
  :override #'lsp-modeline--check-code-actions
  (when (and (lsp-feature? "textDocument/codeAction")
         (flycheck-overlay-errors-at (point))) ;;; (> (length (lsp-cur-line-diagnostics)) 0)
    (lsp-request-async
     "textDocument/codeAction"
     (lsp--text-document-code-action-params)
     #'lsp--modeline-update-code-actions
     :mode 'unchanged
     :cancel-token :lsp-modeline-code-actions)))

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

;;; When file a temp buffer, such magit chunk buffer, flycheck will throw a error:
;;; Suspicious state from syntax checker javascript-eslint: Flycheck checker javascript-eslint returned 2, but its output contained no errors:
;;; Oops! Something went wrong! :(
;;; NOTE Here disable it when file not exist
(defadvice! +flycheck-buffer ()
  :override #'flycheck-buffer
  (interactive)
  (flycheck-clean-deferred-check)
  (when (buffer-file-name)
      (if flycheck-mode
          (unless (flycheck-running-p)
            ;; Clear error list and mark all overlays for deletion.  We do not
            ;; delete all overlays immediately to avoid excessive re-displays and
            ;; flickering, if the same errors gets highlighted again after the check
            ;; completed.
            (run-hooks 'flycheck-before-syntax-check-hook)
            (flycheck-clear-errors)
            (flycheck-mark-all-overlays-for-deletion)
            (condition-case err
                (let* ((checker (flycheck-get-checker-for-buffer)))
                  (if checker
                      (flycheck-start-current-syntax-check checker)
                    (flycheck-clear)
                    (flycheck-report-status 'no-checker)))
              (error
               (flycheck-report-failed-syntax-check)
               (signal (car err) (cdr err)))))
        (user-error "Flycheck mode disabled"))))

;;; Here can not identify +flycheck-buffer's situation
(defun flycheck-disable-on-temp-buffers ()
  (unless (and buffer-file-name (file-exists-p buffer-file-name)) (flycheck-mode -1)))
(add-hook 'prog-mode-hook 'flycheck-disable-on-temp-buffers)

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
;; (use-package! pinyinlib)
;; (defun completion--regex-pinyin (str)
;;   (orderless-regexp (pinyinlib-build-regexp-string str)))
;; (after! orderless
;;  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

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

;; fix emacs daemon company-box no icon
(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (setq company-box-icons-all-the-icons
                      (let ((all-the-icons-scale-factor 0.8))
          `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
            (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
            (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
            (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
            (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
            (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
            (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
            (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
            (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
            (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
            (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
            (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
            (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
            (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
            (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
            (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
            (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
            (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
            (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
            (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
            (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
            (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))
            (ElispFunction . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
            (ElispVariable . ,(all-the-icons-material "check_circle"             :face 'all-the-icons-blue))
            (ElispFeature  . ,(all-the-icons-material "stars"                    :face 'all-the-icons-orange))
            (ElispFace     . ,(all-the-icons-material "format_paint"             :face 'all-the-icons-pink))))))))


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

(after! org-roam
  (require 'org-roam-dailies) ;; Ensure the keymap is available

  (setq org-roam-capture-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                          "${title}\n")
       :unnarrowed t)))

  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  (defun my/org-roam-list-nodes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))
  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-nodes-by-tag "Project")))

  (my/org-roam-refresh-agenda-list)

  (defun my/org-roam-project-finalize-hook ()
    "Add the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "Project")
     :templates
     '(("p" "project") plain "* Goals\n\n%?\n\n** TODO Add initial tasks\n\n* Dates\n\n"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "${title}\n#+category: ${title}\n#+filetags: Project")
       :unnarrowed t)))

  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

  (defun my/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Capture the new tasks, creating the project file if necessary
    (org-roam-capture-
     :node (org-roam-node-read
            nil
            (my/org-roam-filter-by-tag "Project"))
     :templates '(("p" "project" plain "** TODO %?"
                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                          "${title}\n#+category: ${title}\n#+filetags: Project"
                                          ("Tasks"))))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "%<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))
  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))

  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (map! :leader
        "n r p" #'my/org-roam-find-project
        "n r b" #'my/org-roam-capture-inbox
        "n r t" #'my/org-roam-capture-task
        "n r I" #'org-roam-node-insert-immediate))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; @see https://github.com/johnsoncodehk/volar/issues/1118
(defadvice! +lsp--create-filter-function (workspace)
  :override #'lsp--create-filter-function
  (let ((body-received 0)
        leftovers body-length body chunk)
    (lambda (_proc input)
      (setf chunk (if (s-blank? leftovers)
                      input
                    (concat leftovers input)))

      (let (messages)
        (while (not (s-blank? chunk))
          (if (not body-length)
              ;; Read headers
              (if-let ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                  ;; We've got all the headers, handle them all at once:
                  (setf body-length (lsp--get-body-length
                                     (mapcar #'lsp--parse-header
                                             (split-string
                                              (substring-no-properties chunk
                                                                       (or (string-match-p "Content-Length" chunk)
                                                                           (error "Unable to find Content-Length header."))
                                                                       body-sep-pos)
                                              "\r\n")))
                        body-received 0
                        leftovers nil
                        chunk (substring-no-properties chunk (+ body-sep-pos 4)))

                ;; Haven't found the end of the headers yet. Save everything
                ;; for when the next chunk arrives and await further input.
                (setf leftovers chunk
                      chunk nil))
            (let* ((chunk-length (string-bytes chunk))
                   (left-to-receive (- body-length body-received))
                   (this-body (if (< left-to-receive chunk-length)
                                  (prog1 (substring-no-properties chunk 0 left-to-receive)
                                    (setf chunk (substring-no-properties chunk left-to-receive)))
                                (prog1 chunk
                                  (setf chunk nil))))
                   (body-bytes (string-bytes this-body)))
              (push this-body body)
              (setf body-received (+ body-received body-bytes))
              (when (>= chunk-length left-to-receive)
                (condition-case err
                    (with-temp-buffer
                      (apply #'insert
                             (nreverse
                              (prog1 body
                                (setf leftovers nil
                                      body-length nil
                                      body-received nil
                                      body nil))))
                      (decode-coding-region (point-min)
                                            (point-max)
                                            'utf-8)
                      (goto-char (point-min))
                      (while (search-forward "\\u0000" nil t)
                        (replace-match "" nil t))
                      (goto-char (point-min))
                      (push (lsp-json-read-buffer) messages))
                  (error
                   (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                             (concat leftovers input)
                             err)))))))
        (mapc (lambda (msg)
                (lsp--parser-on-message msg workspace))
              (nreverse messages))))))

(defadvice! +lsp-completion--company-match (candidate)
  :override #'lsp-completion--company-match
  (let* ((prefix (downcase
                  (buffer-substring-no-properties
                   (or (plist-get (text-properties-at 0 candidate) 'lsp-completion-start-point) (point))
                   (point))))
         (prefix-len (length prefix))
         (prefix-pos 0)
         (label (downcase candidate))
         (label-len (length label))
         (label-pos 0)
         matches start)
    (while (and (not matches)
                (< prefix-pos prefix-len))
      (while (and (< prefix-pos prefix-len)
                  (< label-pos label-len))
        (if (equal (aref prefix prefix-pos) (aref label label-pos))
            (progn
              (unless start (setq start label-pos))
              (cl-incf prefix-pos))
          (when start
            (setq matches (nconc matches `((,start . ,label-pos))))
            (setq start nil)))
        (cl-incf label-pos))
      (when start (setq matches (nconc matches `((,start . ,label-pos)))))
      ;; Search again when the whole prefix is not matched
      (when (< prefix-pos prefix-len)
        (setq matches nil))
      ;; Start search from next offset of prefix to find a match with label
      (unless matches
        (cl-incf prefix-pos)
        (setq label-pos 0)))
    matches))

(when (display-graphic-p)
  (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0))))
(add-hook! 'doom-first-buffer-hook
  (defun +my/change-cjk-font ()
    "change the cjk font and its size to align the org/markdown tables when have
cjk characters. Font should be twice the width of asci chars so that org tables align.
This will break if run in terminal mode, so use conditional to only run for GUI."
    (when (display-graphic-p)
      (setq user-cjk-font
            (cond
             ((find-font (font-spec :name "Hiragino Sans GB")) "Hiragino Sans GB") ; for macos
             ((find-font (font-spec :name "Noto Sans CJK SC")) "Noto Sans CJK SC") ; for linux
             ))
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset (font-spec :family user-cjk-font
                                             :size 16))))))

(defun xwidget-webkit-search-forward (text)
  "Search forward of `text'"
  (interactive "sSearch: " xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.find(\"%s\");" text)))

(defun xwidget-webkit-test ()
  "Search forward of `text'"
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "
(() => {
  const alphabet = 'abcdefghijklmnopqrstuvwxyz'.split('')
  const currentLinkItems = []

  function getNextKeyCombination(index) {
    let halfIndex = Math.floor(alphabet.length / 2);
    if (index < halfIndex) {
      return alphabet[index];
    } else {
      index -= halfIndex;
      return alphabet[Math.floor(index / alphabet.length) + halfIndex] + alphabet[index % alphabet.length];
    }
  }

  function createLinkItem (link, rect, key) {
    var item = document.createElement('span')
    item.setAttribute('style', 'position: absolute; padding: 1px 3px 0px 3px; background-color: yellow; color: black; z-index: 9999; font-family: Helvetica, Arial, sans-serif;font-weight: bold;font-size: 12px; background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);')

    item.textContent = key

    item.style.top = (window.scrollY + rect.top) + 'px'
    item.style.left = (window.scrollX + rect.left) + 'px'

    return item
  }

  function isVisible (rect) {
    return (
      rect.top > 0 &&
        rect.top < window.innerHeight &&
        rect.left > 0 &&
        rect.left < window.innerWidth
    )
  }

  function showLinkKeys() {
    const links = [];
    const linkRects = [];

    [].slice.call(document.querySelectorAll('a, button, input, textarea, select')).forEach(function (link) {
      var rect = link.getBoundingClientRect()
      if (isVisible(rect)) {
        links.push(link)
        linkRects.push(rect)
      }
    })

    links.forEach(function (link, i) {
      var key = getNextKeyCombination(currentLinkItems.length)
      var item = createLinkItem(link, linkRects[i], key)
      currentLinkItems.push({
        link: link,
        element: item,
        key: key
      })
      document.body.appendChild(item)
    })
  }
  showLinkKeys();
})();
"))
