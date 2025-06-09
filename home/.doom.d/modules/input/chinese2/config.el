;;; input/chinese/config.el -*- lexical-binding: t; -*-

;; (file-truename (concat invocation-directory invocation-name))
;; (file-truename invocation-directory)

;; (shell-command-to-string "readlink ~/.nix-profile/bin/emacs")
(use-package! rime
  :bind
  ("M-j" . #'+rime-convert-string-at-point)
  ;; ("C-SPC" . #'toggle-input-method)
  (:map rime-active-mode-map
    ("M-j" . #'rime-inline-ascii))
  (:map rime-mode-map
    ("C-`" . #'rime-send-keybinding))
  :custom
  (default-input-method "rime")
  (rime-emacs-module-header-root "/opt/homebrew/Cellar/emacs-plus@30/30.1/include")
  ;; (rime-librime-root "~/.doom.d/librime/dist")
  (rime-inline-ascii-trigger 'shift-l)
  (rime-show-candidate 'posframe)
  (rime-librime-root "/opt/homebrew/opt/librime")
  ;; (rime-user-data-dir "~/Library/Rime")
  :config
  ;; and (display-graphic-p)
  (when (find-font (font-spec :name "Hiragino Sans GB"))
    (setq rime-posframe-properties '(:internal-border-width 10 :font "Hiragino Sans GB-17")))
  ;; (aref (font-info "Hiragino Sans GB-18") 3)
  ;; (font-spec :family "Noto Serif" :size 17)

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
                  rime-predicate-prog-in-code-p
                  rime-predicate-in-code-string-p))
  (setq-default rime-inline-predicates
                '(rime-predicate-space-after-cc-p))


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

  (defun treesit-in-string-or-comment-p ()
    "Check if point is inside a string or comment using Tree-sitter for Python."
    (interactive)
    (let* ((node (treesit-node-at (point)))
           (type (treesit-node-type node))
           (in-string (or (string-equal type "string")
                          (string-equal type "string_fragment")))
           (in-comment (string-equal type "comment")))
      (if (or in-string in-comment)
          (message "Point is inside a %s" (if in-string "string" "comment"))
        (message "Point is not inside a string or comment"))))

  (defadvice! +rime-predicate-prog-in-code-p ()
    :override #'rime-predicate-prog-in-code-p
    "If cursor is in code.
Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
    (and (derived-mode-p 'prog-mode 'conf-mode)
         (not (or (and (nth 3 (syntax-ppss)) (inside-string-p))
                  (nth 4 (syntax-ppss)))))))

;;; 修复在 swiper-isearch 中使用时 delete-region 会删除超出区域的内容，导致 args-out-of-range 错误
(defadvice! +rime--minibuffer-message (string)
  :override #'rime--minibuffer-message
  "Concatenate STRING and minibuffer contents.

Used to display in minibuffer when we are using input method in minibuffer."
  (message nil)
  (unless (string-blank-p string)
    (let ((inhibit-quit t)
          point-1)
      (save-excursion
        (insert (concat " |" string)) ;; 当配合 ivy 使用时，换行写入的内容会被搜索结果覆盖，因此改成行内提示
        (setq point-1 (point)))
      (sit-for 1000000)
      (delete-region (point) (min point-1 (point-max)))
      (when quit-flag
        (setq quit-flag nil
              unread-command-events '(7))))))
