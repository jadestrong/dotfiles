;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
;; js2-mode config
;; (add-hook! js2-mode
;;   (setq js2-basic-offset 4
;;         js-indent-level 4
;;         js-switch-indent-offset 4)
;;   )

(after! js2-mode
  (define-key js2-mode-map (kbd "C-c j") 'js-doc-insert-function-doc)
  (define-key js2-mode-map (kbd "@") 'js-doc-insert-tag)
)

;; (defun js2-mode-use-eslint-indent ()
;;   (let ((json-object-type 'hash-table)
;;     (json-config (shell-command-to-string (format  "eslint --print-config %s"
;;                                (shell-quote-argument
;;                             (buffer-file-name))))))
;;     (ignore-errors
;;       (setq js-indent-level
;;         (aref (gethash "indent" (gethash  "rules" (json-read-from-string json-config))) 1)))))

;; (add-hook! js2-mode #'js2-mode-use-eslint-indent)

;; web-mode
(add-hook! web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-comment-style 2
        web-mode-enable-auto-quoting nil
        web-mode-content-types-alist '(("jsx" . ".*\\.js\\'"))))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . rjsx-mode))
;; 设置jsx中html的缩进
(add-hook! rjsx-mode
  (setq sgml-basic-offset 2))

(setq-default
 user-full-name "JadeStrong"
 user-mail-address "jadestrong@163.com"

 doom-font (font-spec :family "Monaco" :size 16))

;; ivy
(map!
 (:after ivy
   (:map ivy-minibuffer-map
     "RET" #'ivy-alt-done))
 )
;; avy
(map!
 (:after avy
   "M-g g" #'avy-goto-line
   "M-g M-g" #'avy-goto-line
   )
 )
;; osx-keys
(when IS-MAC
  ;; (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))
;; 键入命令时关闭中文输入法
(defun mac-selected-keyboard-input-source-change-hook-func ()
  ;; 入力モードが英語の時はカーソルの色をfirebrickに、日本語の時はblackにする
  (set-cursor-color (if (string-match "\\.US$" (mac-input-source))
                        "firebrick" "black")))

(add-hook 'mac-selected-keyboard-input-source-change-hook
          'mac-selected-keyboard-input-source-change-hook-func)

;; ミニバッファにカーソルを移動する際、自動的に英語モードにする
(mac-auto-ascii-mode 1)

;;; "Emacs 25.1 を EMP版で快適に使う"
;;; http://qiita.com/takaxp/items/a86ee2aacb27c7c3a902
;;;
;;; mac-auto-ascii-mode が Enable かつ日本語入力 ON の時、
;;; M-x や C-x C-f 等の後に日本語入力 OFF になる問題を救う。

(defvar mac-win-last-ime-status 'off) ;; {'off|'on}

(defconst mac-win-kana-input-method "com.google.inputmethod.Japanese.base")

(defun advice:mac-auto-ascii-setup-input-source (&optional _prompt)
  "Extension to store IME status"
  (mac-win-save-last-ime-status))

(advice-add 'mac-auto-ascii-setup-input-source :before
            #'advice:mac-auto-ascii-setup-input-source)

(defun mac-win-save-last-ime-status ()
  (setq mac-win-last-ime-status
        (if (string-match "\\.\\(Roman\\|US\\)$" (mac-input-source))
            'off 'on)))

(defun mac-win-restore-ime ()
  (if (mac-win-need-restore-ime)
      (mac-select-input-source mac-win-kana-input-method)))

(defun mac-win-need-restore-ime ()
  (and mac-auto-ascii-mode (eq mac-win-last-ime-status 'on)))

;; M-x 等でミニバッファから元のバッファに戻った後に、日本語入力状態を
;; リストアする。
(add-hook 'minibuffer-setup-hook 'mac-win-save-last-ime-status)
(add-hook 'minibuffer-exit-hook 'mac-win-restore-ime)

;; evil-undo
(setq evil-want-fine-undo 'fine)

;; disable company for org-mode git-mode
(setq company-global-modes '(not org-mode git-commit-mode))
