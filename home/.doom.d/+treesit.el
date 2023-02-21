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
  :mode "\\.mts\\'"
  ;; :init
  ;; (add-to-list 'auto-mode-alist
  ;;              (cons "\\.tsx\\'" #'tsx-ts-mode))
  :config
  (defadvice! +typescript-ts-mode--indent-rules (language)
    :override #'typescript-ts-mode--indent-rules
    `((,language
       ((parent-is "program") point-min 0)
       ((node-is "}") parent-bol 0)
       ((node-is ")") parent-bol 0)
       ((node-is "]") parent-bol 0)
       ((node-is ">") parent-bol 0)
       ((and (parent-is "comment") c-ts-common-looking-at-star)
        c-ts-common-comment-start-after-first-star -1)
       ((parent-is "comment") prev-adaptive-prefix 0)
       ((parent-is "ternary_expression") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "member_expression") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "named_imports") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "statement_block") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "type_arguments") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "variable_declarator") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "arguments") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "array") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "formal_parameters") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "template_substitution") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "object_pattern") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "object") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "object_type") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "enum_body") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "class_body") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)
       ((parent-is "binary_expression") parent-bol typescript-ts-mode-indent-offset)

       ,@(when (eq language 'tsx)
           `(((match "<" "jsx_fragment") parent 0)
             ((parent-is "jsx_fragment") parent typescript-ts-mode-indent-offset)
             ((node-is "jsx_closing_element") parent 0)
             ((node-is "jsx_element") parent typescript-ts-mode-indent-offset)
             ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
             ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
             ((parent-is "jsx_expression") parent-bol typescript-ts-mode-indent-offset)
             ((match "/" "jsx_self_closing_element") parent 0)
             ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset)
             ((parent-is "jsx_text") grand-parent typescript-ts-mode-indent-offset)))
       (no-node parent-bol 0))))
  ;; (setq-local treesit-simple-indent-rules
  ;;   `((tsx
  ;;      ((parent-is "program") parent-bol 0)
  ;;      ((node-is "}") parent-bol 0)
  ;;      ((node-is ")") parent-bol 0)
  ;;      ((node-is "]") parent-bol 0)
  ;;      ((node-is ">") parent-bol 0)
  ;;      ((and (parent-is "comment") comment-end) comment-start -1)
  ;;      ((parent-is "comment") comment-start-skip 0)
  ;;      ((parent-is "ternary_expression") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "member_expression") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "named_imports") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "statement_block") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "type_arguments") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "variable_declarator") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "arguments") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "array") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "formal_parameters") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "template_substitution") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "object_pattern") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "object") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "object_type") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "enum_body") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)

  ;;      ;; TSX
  ;;      ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
  ;;      ((node-is "jsx_closing_element") parent 0)
  ;;      ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
  ;;      ((node-is "/") parent 0)
  ;;      ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset)
  ;;      ((parent-is "jsx_text") grand-parent typescript-ts-mode-indent-offset)
  ;;      (no-node parent-bol 0)))
  ;;   "Tree-sitter indent rules.")
  ;; (setq-local treesit-simple-indent-rules typescript-ts-mode--indent-rules)
  ;; (add-hook! '(typescript-ts-mode-l))
  )

(defun tsx-mode-comment-or-uncomment-region (beg end &optional arg)
  (let ((node_type (treesit-node-type (treesit-node-on beg end))))
    (cond ((string= node_type "jsx_element")
           (message "here")
           (setq-local comment-end "*/}")
           (setq-local comment-start "{/*")
           (funcall 'comment-region-default beg end arg))
          (t (message "default %s" node_type)))))

(add-hook! tsx-ts-mode-hook
  (setq comment-region-function 'tsx-mode-comment-or-uncomment-region))

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
