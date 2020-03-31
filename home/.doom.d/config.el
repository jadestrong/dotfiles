;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;;
;;;;;js2-mode
(after! js2-mode
  (define-key js2-mode-map (kbd "C-c j") 'js-doc-insert-function-doc)
  (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag)
  )

;;;; js-mode
(add-hook 'js-mode-hook 'js2-minor-mode)

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
  ;; (setq company-idle-delay 0.2)
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
  (setq lsp-ui-sideline-show-code-actions nil) ;; 禁用可执行提醒，如 refactor remove 等
  ;; (setq lsp-ui-doc-enable t)
  (setq lsp-eldoc-prefer-signature-help nil
        lsp-eldoc-enable-signature-help nil
        lsp-eldoc-enable-hover t)
  (cond ((and (equal mode-name "Web") (equal web-mode-content-type "vue")) ;; 放在lsp-ui-mode-hook 里面是因为它比web-mode 执行晚，否则 lsp-prefer-flymake 会又被覆盖
         (my/web-vue-setup)))
  )

(defun my/web-vue-setup()
  "Setup for js related."
  (setq-local lsp-prefer-flymake :none)
  ;; (setq company-backends (remove 'company-css company-backends))
  ;; (setq company-backends (remove 'company-web-html company-backends))
  ;; (setq company-backends (remove 'company-lsp company-backends))
  ;; (setq company-backends (remove 'company-yasnippet company-backends))
  )


(after! lsp-mode
  (setq lsp-log-io nil) ;; 开启log,每个project开启一个单独的lsp-log
  )

;;;; font-size
(setq-default doom-font (font-spec :family "Monaco" :size 14))

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

;; evil-matchit
(global-evil-matchit-mode 1)
(evilmi-load-plugin-rules '(web-mode) '(simple template html))
(evilmi-load-plugin-rules '(html-mode) '(simple template html))

;; disable deft auto save
(setq deft-auto-save-interval 0)

;;;; rust-mode
;; (after! rustic
;;   (setq rustic-lsp-server 'rust-analyzer))

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

;; (use-package leetcode-emacs
;;   :config
;;   (setq leetcode-path "~/Dropbox/Leetcode/"
;;         leetcode-language "javascript")
;;   )

;; (setq url-debug t)

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

;;;;; mmm-mako
;; (setq mmm-global-mode 'maybe)
;; (use-package! mmm-mako)
(add-to-list 'auto-mode-alist '("\\.mako\\'" . web-mode))
;; (use-package! mmm-mode
;;   :config
;;   (mmm-add-mode-ext-class 'html-mode "\\.mako\\'" 'mako))

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
  ;; (rime-share-data-dir
  ;;  (cl-some (lambda (dir)
  ;;             (let ((abs-dir (expand-file-name dir)))
  ;;               (when (file-directory-p abs-dir)
  ;;                 abs-dir)))
  ;;           (cond (IS-MAC
  ;;                  '("~/Library/Rime"
  ;;                    "/Library/Input Methods/Squirrel.app/Contents/SharedSupport"))
  ;;                 (IS-LINUX
  ;;                  '("~/.config/ibus/rime"
  ;;                    "~/.config/fcitx/rime"
  ;;                    "/usr/share/local"
  ;;                    "/usr/share")))))
  (rime-inline-ascii-trigger 'shift-l)
  ;; :hook
  ;; ('after-init . (lambda ()
  ;;                  (when (fboundp 'rime-lib-sync-user-data)
  ;;                    (ignore-errors (rime-sync)))))
  ;; ('kill-emacs . (lambda ()
  ;;                  (when (fboundp 'rime-lib-sync-user-data)
  ;;                    (ignore-errors (rime-sync)))))
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
  ;; (unless (fboundp 'rime--posframe-display-content)
  ;;     (error "Function `rime--posframe-display-content' is not available."))
  ;; (defadvice! +rime--posframe-display-content-a (args)
  ;;   "给 `rime--posframe-display-content' 传入的字符串加一个全角空
  ;; 格，以解决 `posframe' 偶尔吃字的问题。"
  ;;   :filter-args #'rime--posframe-display-content
  ;;   (cl-destructuring-bind (content) args
  ;;     (let ((newresult (if (string-blank-p content)
  ;;                          content
  ;;                        (concat content "　"))))
  ;;       (list newresult))))
  ;; (load! "+rime-predicates")
  (setq-default rime-disable-predicates
                '(rime-predicate-evil-mode-p
                  ;; rime-predicate-ace-window-mode-p
                  ;; rime-predicate-punctuation-line-begin-p
                  rime-predicate-after-alphabet-char-p
                  rime-predicate-prog-in-code-p
                  ;; rime-predicate-auto-english-p
                  ;; +rime-predicate-beancount-p
                  ))
  ;; (setq-default rime-inline-predicates
  ;;               '(rime-predicate-evil-mode-p
  ;;                 rime-predicate-punctuation-line-begin-p
  ;;                 rime-predicate-after-alphabet-char-p
  ;;                 rime-predicate-prog-in-code-p))
  (setq-default rime-inline-predicates
                '(rime-predicate-space-after-cc-p))
  )

(use-package! insert-translated-name)

(defun org-brain-deft ()
  "Use `deft' for files in `org-brain-path'."
  (interactive)
  (let ((deft-directory org-brain-path)
        (deft-recursive t)
        (deft-extensions '("org")))
    (deft)))
