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
