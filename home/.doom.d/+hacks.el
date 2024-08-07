;;; ../dotfiles/home/.doom.d/+hacks.el -*- lexical-binding: t; -*-

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

;; 走 server 使用原来的 base64 总是失败，更改为走 hex 编码 https://plantuml.com/zh/text-encoding
;; (defun encode-string-hex (string)
;;   (let ((res nil))
;;     (dolist (num (string-to-list string) (apply #'concat (reverse res)))
;;       (push (format "%x" num) res))))

(defun my/hex-encode-string (ascii-string)
  (let ((res nil))
    (dotimes (i (length ascii-string) (apply #'concat (reverse res)))
      (let ((ascii-char (substring ascii-string i  (+ i 1))))
        (push (format "%.2x" (string-to-char ascii-char)) res)))))

(defadvice! +plantuml-server-encode-url (string)
  :override #'plantuml-server-encode-url
  (let* ((coding-system (or buffer-file-coding-system
                            "utf8"))
         (encoded-string (my/hex-encode-string (encode-coding-string string 'utf-8))))
    (concat plantuml-server-url "/" plantuml-output-type "/~h" encoded-string)))

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

;; 当打开压缩后的 js 文件时， dtrt-indent-mode 会因为单行内容太长，生成的正则太长而报错，这里忽略其抛出的错误
(defadvice! +doom-detect-indentation-h nil
  :override #'doom-detect-indentation-h
  (unless (or (not after-init-time)
              doom-inhibit-indent-detection
              doom-large-file-p
              (memq major-mode doom-detect-indentation-excluded-modes)
              (member (substring (buffer-name) 0 1) '(" " "*")))
    ;; Don't display messages in the echo area, but still log them
    (let ((inhibit-message (not init-file-debug)))
      (ignore-error
          (dtrt-indent-mode +1)))))

;; delete-char will trigger a change event, whick cause lsp-on-change execute a lot.
(defadvice! +popup-delete (popup)
  :override #'popup-delete
  (when (popup-live-p popup)
    (popup-hide popup)
    (mapc 'delete-overlay (popup-overlays popup))
    (setf (popup-overlays popup) nil)
    (setq popup-instances (delq popup popup-instances))
    ;; Restore newlines state
    (let ((newlines (popup-newlines popup)))
      (when (> newlines 0)
        (popup-save-buffer-state
          (goto-char (point-max))
          (dotimes (i newlines)
            (if (and (char-before)
                     (= (char-before) ?\n))
                ;; (delete-char -1)
                (message "try delete-char")))))))
  nil)


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


;;; @see https://github.com/johnsoncodehk/volar/issues/1118
;; (defadvice! +lsp--create-filter-function (workspace)
;;   :override #'lsp--create-filter-function
;;   (let ((body-received 0)
;;         leftovers body-length body chunk)
;;     (lambda (_proc input)
;;       (setf chunk (if (s-blank? leftovers)
;;                       input
;;                     (concat leftovers input)))

;;       (let (messages)
;;         (while (not (s-blank? chunk))
;;           (if (not body-length)
;;               ;; Read headers
;;               (if-let ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
;;                   ;; We've got all the headers, handle them all at once:
;;                   (setf body-length (lsp--get-body-length
;;                                      (mapcar #'lsp--parse-header
;;                                              (split-string
;;                                               (substring-no-properties chunk
;;                                                                        (or (string-match-p "Content-Length" chunk)
;;                                                                            (error "Unable to find Content-Length header."))
;;                                                                        body-sep-pos)
;;                                               "\r\n")))
;;                         body-received 0
;;                         leftovers nil
;;                         chunk (substring-no-properties chunk (+ body-sep-pos 4)))

;;                 ;; Haven't found the end of the headers yet. Save everything
;;                 ;; for when the next chunk arrives and await further input.
;;                 (setf leftovers chunk
;;                       chunk nil))
;;             (let* ((chunk-length (string-bytes chunk))
;;                    (left-to-receive (- body-length body-received))
;;                    (this-body (if (< left-to-receive chunk-length)
;;                                   (prog1 (substring-no-properties chunk 0 left-to-receive)
;;                                     (setf chunk (substring-no-properties chunk left-to-receive)))
;;                                 (prog1 chunk
;;                                   (setf chunk nil))))
;;                    (body-bytes (string-bytes this-body)))
;;               (push this-body body)
;;               (setf body-received (+ body-received body-bytes))
;;               (when (>= chunk-length left-to-receive)
;;                 (condition-case err
;;                     (with-temp-buffer
;;                       (apply #'insert
;;                              (nreverse
;;                               (prog1 body
;;                                 (setf leftovers nil
;;                                       body-length nil
;;                                       body-received nil
;;                                       body nil))))
;;                       (decode-coding-region (point-min)
;;                                             (point-max)
;;                                             'utf-8)
;;                       (goto-char (point-min))
;;                       (while (search-forward "\x00" nil t)
;;                         (replace-match "" nil t))
;;                       (goto-char (point-min))
;;                       (push (lsp-json-read-buffer) messages))

;;                   (error
;;                    (lsp-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
;;                              (concat leftovers input)
;;                              err)))))))
;;         (mapc (lambda (msg)
;;                 (lsp--parser-on-message msg workspace))
;;               (nreverse messages))))))


;; 联合 company-dabbrev 使用的时候， (plist-get (text-properties-at 0 candidate) 'lsp-completion-start-point) 有可能是 nil 会报错
;; 临时这么修复一下，会导致在注释里面补全的内容的高亮有问题，不知道怎么解
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

;; 在 document-frame 中显示 detail ，和 vscode 的表现一致
(defadvice! +lsp-completion--get-documentation (item)
  :override #'lsp-completion--get-documentation
  (unless (get-text-property 0 'lsp-completion-resolved item)
    (let ((resolved-item
           (-some->> item
             (get-text-property 0 'lsp-completion-item)
             (lsp-completion--resolve)))
          (len (length item)))
      (put-text-property 0 len 'lsp-completion-item resolved-item item)
      (put-text-property 0 len 'lsp-completion-resolved t item)))
  (when-let* ((completion-item (get-text-property 0 'lsp-completion-item item)))
    (cond ((lsp:completion-item-documentation? completion-item)
           (lsp--render-element (lsp:completion-item-documentation? completion-item)))
          ((lsp:completion-item-detail? completion-item)
           (lsp--render-element (lsp:completion-item-detail? completion-item)))
          (t nil))))

;; (after! lsp-mode
  ;; 优先使用系统按钮的 tsserver
  ;; (setq lsp-clients-typescript-server-args `("--stdio"))
  ;; (setq lsp-clients-typescript-tsserver '((path . "/opt/homebrew/bin/tsserver")))
  ;; (setq lsp-clients-typescript-tsserver nil)
  ;; )

(after! eglot
  (add-to-list 'eglot-server-programs '(typescript-tsx-mode . ("typescript-language-server" "--tsserver-path" "/opt/homebrew/bin/tsserver" "--stdio"))))

;; emacs-rime 有时候中文候选词的字体会变，大部分时候这个方法是解决了，但偶尔还是遇到了，待观察
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

;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root
;; 默认 lsp 会记住所有之前打开的 vue 项目，并每次启动的时候都会在每个项目里面都启用一个 vls 服务，这里强制其遗忘
(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))


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


;; 修复当安装了 git hooks 插件后， magit-process-mode 中输出的内容有颜色时导致的乱码问题
(defun color-buffer (proc &rest args)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))
(advice-add 'magit-process-filter :after #'color-buffer)


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

(when (modulep! :checkers syntax +childframe)
  (defun flycheck-posframe-monitor-post-command ()
    (when (not (flycheck-posframe-check-position))
      (posframe-hide flycheck-posframe-buffer)))

  (defun fix-flycheck-posframe-not-hide-immediately ()
    (cond (flycheck-posframe-mode
           (add-hook 'post-command-hook 'flycheck-posframe-monitor-post-command nil t))
          ((not flycheck-posframe-mode)
           (remove-hook 'post-command-hook 'flycheck-posframe-monitor-post-command t))))
  (add-hook! flycheck-posframe-mode #'fix-flycheck-posframe-not-hide-immediately))


;; 已通过移除 company-ispell 解决，这里其实就不需要了，记录一下
;; disable org-mode company-mode
;; remove company-ispell so can disable this
;; (defun disable-company-hook ()
;;   (company-mode -1))
;; (when (featurep! :completion company)
;;   (add-hook! (org-mode markdown-mode text-mode) 'disable-company-hook))

(set-docsets! 'typescript-tsx-mode "tailwindcss")
(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(after! dash-docs
  (setq dash-docs-browser-func #'+lookup-xwidget-webkit-open-url-fn))
(defadvice! +dash-docs--run-query (docset search-pattern)
  :override #'dash-docs--run-query
  (let* ((docset-name (car docset))
         (docset-type (cond ((string-equal "tailwindcss" docset-name) "DASH")
                            (t (cl-caddr docset)))))
    (dash-docs-sql
     (cadr docset)
     (dash-docs-sql-query docset-type
                          (dash-docs-sub-docset-name-in-pattern search-pattern
                                                                (car docset))))))
;; debounce swiper
(setq ivy-dynamic-exhibit-delay-ms 200)
(defvar +ivy--queue-last-input nil)
(defun +ivy-queue-exhibit-a(f &rest args)
  (if (equal +ivy--queue-last-input (ivy--input))
      (ivy--exhibit)
    (apply f args))
  (setq +ivy--queue-last-input (ivy--input)))
(advice-add 'ivy--queue-exhibit :around #'+ivy-queue-exhibit-a)

;; company-box 当上下分屏时计算的位置不对，导致 frame 始终在下方显示
(defadvice! +company-box--compute-frame-position (frame)
  :override #'company-box--compute-frame-position
  (-let* ((window-configuration-change-hook nil)
          ((left top _right _bottom) (company-box--edges))
          (window-tab-line-height (if (fboundp 'window-tab-line-height)
                                      (window-tab-line-height)
                                    0))
          (top (+ top window-tab-line-height company-box-frame-top-margin))
          (char-height (frame-char-height frame))
          (char-width (frame-char-width frame))
          (height (* (min company-candidates-length company-tooltip-limit) char-height))
          (space-numbers (if (eq company-show-quick-access 'left) char-width 0))
          (frame-resize-pixelwise t)
          (mode-line-y (company-box--point-bottom))
          ((p-x . p-y) (company-box--prefix-pos))
          (p-y-abs (+ top p-y))
          (y (or (and (> p-y-abs (/ mode-line-y 2))
                      (<= (- mode-line-y p-y-abs) (+ char-height height))
                      (> (- p-y-abs height) 0)
                      (- p-y height))
                 (+ p-y char-height)))
          (height (or (and (> y p-y)
                           (> height (- mode-line-y y))
                           (- mode-line-y y))
                      height))
          (height (- height (mod height char-height)))
          (scrollbar-width (if (eq company-box-scrollbar 'left) (frame-scroll-bar-width frame) 0))
          (x (if (eq company-box-frame-behavior 'point)
                 p-x
               (if company-box--with-icons-p
                   (- p-x (* char-width (+ company-box-icon-right-margin (if (= company-box--space 2) 2 3))) space-numbers scrollbar-width)
                 (- p-x (if (= company-box--space 0) 0 char-width) space-numbers scrollbar-width)))))
    (setq company-box--x (max (+ x left) 0)
          company-box--top (+ y top)
          company-box--height height
          company-box--chunk-size (/ height char-height))
    (with-current-buffer (company-box--get-buffer)
      (setq company-box--x (max (+ x left) 0)
            company-box--top (+ y top)
            company-box--height height
            company-box--chunk-size (/ height char-height)))))

;; 让 company-box-doc 在左右空间不足时，在上下显示
(defadvice! +company-box-doc--set-frame-position (frame)
  :override #'company-box-doc--set-frame-position
  (-let* ((box-position (frame-position (company-box--get-frame)))
          (box-width (frame-pixel-width (company-box--get-frame)))
          (box-height (frame-pixel-height (company-box--get-frame)))
          (char-width (frame-char-width (company-box--get-frame)))
          (char-height (frame-char-height (company-box--get-frame)))
          (scrollbar-width (if (and (eq company-box-scrollbar t) (company-box--scrollbar-p (company-box--get-frame))) (* 2 char-width) 0))
          (window (frame-root-window frame))
          (frame-resize-pixelwise t)
          ((width . height)
           (if company-box-doc-no-wrap
               (window-text-pixel-size window nil nil 10000 10000)
             (window-text-pixel-size
              window nil nil
              ;; Use the widest space available (left or right of the box frame)
              (let* ((space-right (- (frame-native-width) (+ 40 (car box-position) box-width)))
                     (space-left (- (car box-position) 40))
                     (max-space (max space-right space-left)))
                ;; (message "max-space %s box-width %s" max-space box-width)
                (if (< max-space 200) box-width max-space))
              (- (frame-native-height) 40))))
          (bottom (+ company-box--bottom (window-pixel-top) (frame-border-width)))
          ;; 默认在右侧的定位点
          (x (+ (car box-position) box-width (/ (frame-char-width) 2)))
          (y (cdr box-position))
          (y (if (> (+ y height 20) bottom)
                 (- y (- (+ y height) bottom) 20)
               y))
          (space-right (- (frame-pixel-width) x))
          (space-left (car box-position))
          ((_edge_left _edge_top _edge_right edge_bottom) (company-box--edges))
          (x (let ((border (* (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0)
                              2)))
               (cond ((> space-right width) x)
                     ((> space-left (+ width border (/ (frame-char-width) 2)))
                      (- (car box-position) width border (/ (frame-char-width) 2)))
                     (t (car box-position)))))
          (y (if (= x (car box-position))
                 (if (< (+ (cdr box-position) box-height height) edge_bottom)
                     (+ y box-height) ;; 下面显示
                   (- (cdr box-position) (min height (- (cdr box-position) 100)) char-height)) ;; 上面显示
               y))
          )
    ;; (message "width %s height %s" width height)
    ;; (message "x %s y %s box-x %s box-y %s" x y (car box-position) (cdr box-position))
    (set-frame-position frame x (if (< y 0) (+ y (cdr box-position)) (max y 10)))
    (set-frame-size frame (if (= x (car box-position)) (- box-width scrollbar-width) width) (if (< y (cdr box-position)) (min height (- (cdr box-position) 100)) height) t)))

;; https://github.com/alexluigit/dirvish/pull/251
(defadvice! +dirvish--mode-line-fmt-setter (left right &optional header)
  :override #'dirvish--mode-line-fmt-setter
  (cl-labels ((expand (segments)
                (cl-loop for s in segments collect
                         (if (stringp s) s
                           `(:eval (,(intern (format "dirvish-%s-ml" s)) (dirvish-curr))))))
              (get-font-scale ()
                (let* ((face (if header 'header-line 'mode-line-inactive))
                       (defualt (face-attribute 'default :height))
                       (ml-height (face-attribute face :height)))
                  (cond ((floatp ml-height) ml-height)
                        ((integerp ml-height) (/ (float ml-height) defualt))
                        (t 1)))))
    `((:eval
       (let* ((dv (dirvish-curr))
              (buf (and (car (dv-layout dv)) (cdr (dv-index dv))))
              (scale ,(get-font-scale))
              (win-width (floor (/ (window-width) scale)))
              (str-l (format-mode-line
                      ',(or (expand left) mode-line-format) nil nil buf))
              (str-r (format-mode-line ',(expand right) nil nil buf))
              (len-r (string-width str-r)))
         (concat
          (dirvish--bar-image (car (dv-layout dv)) ,header)
          (if (< (+ (string-width str-l) len-r) win-width)
              str-l
            (let ((trim (1- (- win-width len-r))))
              (if (>= trim 0)
                  (substring str-l 0 (min trim (1- (length str-l))))
                "")))
          (propertize
           " " 'display
           `((space :align-to (- (+ right right-fringe right-margin)
                                 ,(ceiling (* scale (string-width str-r)))))))
          str-r))))))
