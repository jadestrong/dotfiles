;;; ../dotfiles/home/.doom.d/+treesit.el -*- lexical-binding: t; -*-

(defun treesit-node-at-pos (&optional node-type pos)
  (let* ((root (treesit-buffer-root-node))
         (p (or pos (point)))
         (node (treesit-node-descendant-for-range root p p (eq node-type :named))))
    (pcase node-type
      ('nil node)
      (:named node)
      (:anonymous (unless (treesit-node-check node 'named) node))
      (_ (treesit-parent-until node (lambda (this) (equal node-type (intern (treesit-node-type this)))))))))

(defun ts-mode--jsx-self-closing-tag-at-point-p ()
  (save-excursion
    (re-search-backward "[^\r\n[:space:]]" nil t)
    (let* ((last-named-node (treesit-node-at-pos :named))
           (last-named-node-type (when last-named-node (treesit-node-type last-named-node)))
           (last-anon-node (treesit-node-at-pos :anonymous))
           (last-anon-node-type (when last-anon-node (treesit-node-text last-anon-node))))
      (or (string= last-anon-node-type "=>")
          (string= last-anon-node-type "(")
          (string= last-anon-node-type "?")
          (string= last-anon-node-type "[")
          (string= last-anon-node-type ",")
          (string= last-anon-node-type "=")
          (string= last-named-node-type "jsx_opening_element")
          (string= last-named-node-type "jsx_closing_element")
          (string= last-named-node-type "jsx_self_closing_element")
          (string= last-named-node-type "jsx_fragment")
          (string= last-named-node-type "jsx_expression")))))

(defun ts-mode-jsx-maybe-insert-self-closing-tag ()
  "When `tsx-mode-tsx-auto-tags' is non-nil, insert a self-closing element
instead of a plain '<' character (where it makes sense to)."
  (interactive)
  (if (or (bobp)
          (ts-mode--jsx-self-closing-tag-at-point-p))
      (progn
        (insert "</>")
        (backward-char 2))
    (insert "<")))

(defun ts-mode--jsx-tag-convert-at-point-p ()
  "Internal function.
Return t if a self-closing tag at point can be turned into an opening tag and a
closing tag."
  (or
   (when-let* ((current-named-node (treesit-node-at-pos :named))
               (current-named-node-type (treesit-node-type current-named-node)))
     ;; self-closing tags can be turned into regular tag sets
     ;; (message "current-named %s" current-named-node-type)
     (string= current-named-node-type "jsx_self_closing_element"))
   (save-excursion
     ;; a "</>" string inserted via `tsx-mode-auto-tags' can be turned into
     ;; a set of JSX fragment tags
     (backward-char 1)
     (looking-at-p "</>"))))

;; (defun ts-mode--jsx-node-opening-tag-name (node)
;;   (let ((tag (treesit-node-text node)))
;;     (message "tag %s" tag)
;;     (cond
;;      ((string= tag "/") "")
;;      (t tag))))

(defun ts-mode-jsx-maybe-close-tag ()
  "When `tsx-mode-tsx-auto-tags' is non-nil, turn the current self-closing tag
(if any) into a regular tag instead of inserting a plain '>' character."
  (interactive)
  (if (ts-mode--jsx-tag-convert-at-point-p)
      (let* ((node-element-name
              (save-excursion
                (goto-char (- (treesit-node-start (treesit-node-at-pos :named)) 1))
                ;; TODO: there should be a way to use [:word:] here right?
                (re-search-forward "<\\([-a-zA-Z0-9$_.]*\\)" nil t)
                ;; (tsx-mode--debug "tag match: %s" (match-data))
                (match-string 1)))
             ;; the above will calculate the name of a fragment as "/"
             (str (format "></%s>" (if (string= node-element-name "/") "" node-element-name))))
        (re-search-forward "/>" nil t)
        (delete-char -2)
        (insert str)
        (backward-char (- (length str) 1)))
    (insert ">")))

(after! typescript-ts-mode
  (define-key tsx-ts-mode-map (kbd "<") 'ts-mode-jsx-maybe-insert-self-closing-tag)
  (define-key tsx-ts-mode-map (kbd ">") 'ts-mode-jsx-maybe-close-tag))

(use-package! typescript-ts-mode
  :mode "\\.mts\\'")

(defun jsx-context-p ()
  "Move cursor to the enclosing JSX element."
  (interactive)
  (let ((node (treesit-node-at (point)))
        (found nil))
    (while (and node (not found))
      (setq node (treesit-node-parent node))
      (message "node type %s" (treesit-node-type node))
      (when (and node
                 (equal (treesit-node-type node) "jsx_element"))
        (setq found t)))
    found))

(defun check_node ()
  (let ((origin-comment-start comment-start)
        (origin-comment-end comment-end)
        (node (treesit-node-at (point)))
        (matched nil))
    (while (and node (not matched))
      (setq node (treesit-node-parent node))
      (when node
        (let ((node-type (treesit-node-type node)))
          (cond ((equal node-type "jsx_attribute")
                 (comment-region-default beg end arg)
                 (setq matched t))
                ((equal node-type "jsx_element")
                 (let ((comment-start "{/* ")
                       (comment-end " */}"))
                   (comment-region-default beg end arg)
                   (setq comment-start origin-comment-start)
                   (setq comment-end origin-comment-end)))
                (t (comment-region-default beg end arg)))))))
  )

(defun tsx-mode-comment-region (beg end &optional arg)
  ;; (message "parent %s" (treesit-node-type (treesit-node-descendant-for-range (treesit-buffer-root-node) beg end)))
  ;; (message "test: %s" (treesit-node-type (treesit-node-on beg end)))
  (cond ((equal (treesit-node-type (treesit-node-on beg end)) "parenthesized_expression")
         (comment-region-default beg end arg))
        (t
         (let ((origin-comment-start comment-start)
               (origin-comment-end comment-end)
               (node (treesit-node-at (point)))
               (matched nil))
           (while (and node (not matched))
             (setq node (treesit-node-parent node))
             (when node
               (let ((node-type (treesit-node-type node)))
                 ;; (message "node-type %s" node-type)
                 (cond ((equal node-type "jsx_attribute")
                        (comment-region-default beg end arg)
                        (setq matched t))
                       ((equal node-type "jsx_element")
                        (let ((comment-start "{/* ")
                              (comment-end " */}"))
                          (comment-region-default beg end arg)
                          (setq comment-start origin-comment-start)
                          (setq comment-end origin-comment-end))
                        (setq matched t))
                       (t ())))))
           (unless matched
             (comment-region-default beg end arg)))
         ;; (let ((origin-comment-start comment-start)
         ;;         (origin-comment-end comment-end))
         ;;     (if (jsx-context-p)
         ;;         (let ((comment-start "{/* ")
         ;;               (comment-end " */}"))
         ;;           (comment-region-default beg end arg)
         ;;           (setq comment-start origin-comment-start)
         ;;           (setq comment-end origin-comment-end))
         ;;       (comment-region-default beg end arg)))
         )))

;; copy from hack.el
(defun tsx-mode-uncomment-region (beg end &optional _)
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

  (set-marker end nil))

;;  (let ((node_type (treesit-node-type (treesit-node-on beg end))))
;;    (cond ((string= node_type "jsx_element")
;;           (message "here")
;;           (setq-local comment-end "*/}")
;;           (setq-local comment-start "{/*")
;;           (funcall 'comment-region-default beg end arg))
;;          (t (message "default %s" node_type))))

(add-hook! 'tsx-ts-mode-hook
  (setq-local comment-use-syntax nil)
  (setq-local comment-start-skip "[[:space:]]*\\(//+\\|{?/\\*+\\)")
  ;; \n is included to get arround `comment-normalize-vars' and `comment-only-p'
  (setq-local comment-end-skip "\\(\\*+/}?[[:space:]]*\\)\n?\\|\n")
  (setq-local comment-region-function 'tsx-mode-comment-region)
  (setq-local uncomment-region-function 'tsx-mode-uncomment-region))

(add-hook! '(typescript-ts-base-mode-hook)
  (defun +javascript-init-lsp-h ()
    "Start `lsp' in the current buffer."
    (let ((buffer-file-name (buffer-file-name (buffer-base-buffer))))
      (when (derived-mode-p 'typescript-ts-base-mode)
        (if (null buffer-file-name)
            ;; necessary because `tide-setup' and `lsp' will error if not a
            ;; file-visiting buffer
            (add-hook 'after-save-hook #'+javascript-init-lsp-h
                      nil 'local)
          (if (modulep! :lang javascript +lsp) (lsp!)
            (ignore
             (doom-log "Couldn't start lsp")))
          (remove-hook 'after-save-hook #'+javascript-init-lsp-h
                       'local))))))

(setq treesit–indent-verbose nil)
