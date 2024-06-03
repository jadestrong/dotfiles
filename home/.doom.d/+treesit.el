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

(defun ts-mode-electric-lt (n)
  "Insert a context-sensitive less-than sign.
Optional prefix argument N indicates how many signs to insert.
If N is greater than one, no special handling takes place.
Otherwise, if the less-than sign would start a JSX block, it
inserts `</>' and places the cursor inside the new tag."
    (interactive "p")
    (if (/= n 1)
        (insert (make-string n ?<))
      (if (save-excursion
            (forward-comment most-negative-fixnum)
            (skip-chars-backward "\n\r")
            (or (= (point) (point-min))
                (memq (char-before) (append "=(?:>}&|{," nil))
                (let ((start (- (point) 6)))
                  (and (>= start (point-min))
                       (string= (buffer-substring start (point)) "return")))))
          (progn (insert "</>")
                 (backward-char 2))
        (insert "<"))))

(after! typescript-ts-mode
  ;; (define-key tsx-ts-mode-map (kbd "<") 'ts-mode-jsx-maybe-insert-self-closing-tag)
  (define-key tsx-ts-mode-map (kbd "<") 'ts-mode-electric-lt)
  (define-key tsx-ts-mode-map (kbd ">") 'ts-mode-jsx-maybe-close-tag))

(use-package! typescript-ts-mode
  :mode "\\.mts\\'"
  :config
  ;; (defadvice! +typescript-ts-mode--indent-rules (language)
  ;;   :override #'typescript-ts-mode--indent-rules
  ;;   `((,language
  ;;      ((parent-is "program") point-min 0)
  ;;      ((node-is "}") parent-bol 0)
  ;;      ((node-is ")") parent-bol 0)
  ;;      ((node-is "]") parent-bol 0)
  ;;      ((node-is ">") parent-bol 0)
  ;;      ((and (parent-is "comment") c-ts-common-looking-at-star)
  ;;       c-ts-common-comment-start-after-first-star -1)
  ;;      ((parent-is "comment") prev-adaptive-prefix 0)
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
  ;;      ((parent-is "class_body") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "arrow_function") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "parenthesized_expression") parent-bol typescript-ts-mode-indent-offset)
  ;;      ((parent-is "binary_expression") parent-bol typescript-ts-mode-indent-offset)

  ;;      ,@(when (eq language 'tsx)
  ;;          `(((match "<" "jsx_fragment") parent 0)
  ;;            ((parent-is "jsx_fragment") parent typescript-ts-mode-indent-offset)
  ;;            ((node-is "jsx_closing_element") parent 0)
  ;;            ((node-is "jsx_element") parent typescript-ts-mode-indent-offset)
  ;;            ((parent-is "jsx_element") parent typescript-ts-mode-indent-offset)
  ;;            ((parent-is "jsx_opening_element") parent typescript-ts-mode-indent-offset)
  ;;            ((parent-is "jsx_expression") parent-bol typescript-ts-mode-indent-offset)
  ;;            ((match "/" "jsx_self_closing_element") parent 0)
  ;;            ((parent-is "jsx_self_closing_element") parent typescript-ts-mode-indent-offset)
  ;;            ((parent-is "jsx_text") grand-parent typescript-ts-mode-indent-offset)))
  ;;      (no-node parent-bol 0))))
  )

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

(setq treesit--indent-verbose nil)

;; Copy from https://github.com/llemaitre19/jtsx
(defconst jtsx-jsx-ts-keys '("jsx_expression"
                             "jsx_element"
                             "jsx_attribute"
                             "jsx_self_closing_element"
                             "jsx_text"
                             "jsx_opening_element"
                             "jsx_closing_element"
                             "jsx_namespace_name"))

(defun jtsx-jsx-context-at-p (position &optional jsx-exp-guard)
  "Check if inside JSX context at POSITION.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (when-let ((node (jtsx-treesit-node-at position)))
    (jtsx-enclosing-jsx-node node jtsx-jsx-ts-keys nil t jsx-exp-guard)))

(defun jtsx-jsx-context-p (&optional jsx-exp-guard)
  "Check if in JSX context at point or at region ends.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (and (jtsx-jsx-context-at-p (point) jsx-exp-guard)
       (or (not (region-active-p)) (jtsx-jsx-context-at-p (mark) jsx-exp-guard))))

(defun jtsx-treesit-node-at (pos)
  "Return the treesit node at POS or POS-1 depending on the context.
Treesit looks at the node after the position (excepted when at the end of the
line), it fine in most situations, excepted when at the end of a region where
getting the treesit node before position is more suitable."
  (let ((effective-pos (if (and (region-active-p) (eq pos (region-end))) (1- pos) pos)))
    (treesit-node-at effective-pos)))

(defun jtsx-traversing-jsx-expression-p (node initial-node)
  "Check whether we are going outside a jsx expression.
NODE is the current node, and INITIAL-NODE is the node which the research has
started from.  We consider that we are not traversing the jsx-expression if
one of the INITIAL-NODE and NODE positions matches (start or end), ie we were
already outside it."
  (and (equal (treesit-node-type node) "jsx_expression")
       (not (eq (treesit-node-start initial-node) (treesit-node-start node)))
       (not (eq (treesit-node-end initial-node) (treesit-node-end node)))))

(defun jtsx-enclosing-jsx-node (node types &optional fallback-types include-node jsx-exp-guard)
  "Get first parent of NODE matching one of TYPES.
If the research failed and FALLBACK-TYPES are not nil retry with FALLBACK-TYPES.
If INCLUDE-NODE is not nil, NODE is included in the research.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (let* ((get-parent-node (lambda (current-node)
                           (let ((parent-node (treesit-node-parent current-node)))
                             (if (and jsx-exp-guard
                                      (jtsx-traversing-jsx-expression-p parent-node node))
                                 nil
                               parent-node))))
         (enclosing-node (if include-node node (funcall get-parent-node node))))
    (while (and enclosing-node (not (member (treesit-node-type enclosing-node) types)))
      (setq enclosing-node (funcall get-parent-node enclosing-node)))
    (if (or enclosing-node (not fallback-types))
        enclosing-node
      (jtsx-enclosing-jsx-node node fallback-types nil include-node jsx-exp-guard))))

(defun jtsx-enclosing-jsx-element (node &optional jsx-exp-guard)
  "Get first parent of NODE matching `jsx_element' type.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (jtsx-enclosing-jsx-node node '("jsx_element" "jsx_self_closing_element") nil t jsx-exp-guard))

(defun jtsx-enclosing-jsx-element-at-point (&optional jsx-exp-guard)
  "Get first parent matching `jsx_element' type at point.
If JSX-EXP-GUARD is not nil, do not traverse jsx expression."
  (jtsx-enclosing-jsx-element (jtsx-treesit-node-at (point)) jsx-exp-guard))

(defun jtsx-jump-jsx-opening-tag ()
  "Jump to the opening tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1+ (treesit-node-start enclosing-element))) ; +1 to jump right after the "<"
      (message "No JSX opening element found"))))

(defun jtsx-jump-jsx-closing-tag ()
  "Jump to the closing tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (goto-char (1- (treesit-node-end enclosing-element))) ; -1 to jump right before the "/>"
      (message "No JSX closing element found"))))

(defun jtsx-jump-jsx-element-tag-dwim ()
  "Jump either to the opening or the closing tag of the JSX element."
  (interactive "^")
  (let ((enclosing-element (jtsx-enclosing-jsx-element-at-point)))
    (if enclosing-element
        (let ((start (treesit-node-start enclosing-element))
              (end (treesit-node-end enclosing-element)))
          (if (> (point) (+ start (/ (- end start) 2)))
              (jtsx-jump-jsx-opening-tag) ; We are closer to the closing tag.
            (jtsx-jump-jsx-closing-tag))) ; We are closer to the opening tag.
      (message "No JSX element found"))))

(defun jtsx-inside-empty-inline-jsx-element-p ()
  "Return t if inside an empty inline jsx element.
For example:
<></>
<A></A>
<A
  attribute
></A>"
  (when (jtsx-jsx-context-p)
    (when-let* ((node (jtsx-treesit-node-at (point))))
      (when (equal (treesit-node-type node) "</")
        (when-let* ((element-node (jtsx-enclosing-jsx-element node))
                    (open-tag (treesit-node-child-by-field-name element-node "open_tag"))
                    (close-tag (treesit-node-child-by-field-name element-node "close_tag"))
                    (open-tag-end-line (line-number-at-pos (treesit-node-end open-tag)))
                    (close-tag-start-line (line-number-at-pos (treesit-node-start close-tag)))
                    ;; The function is called after newline insertion, so close tag is one line
                    ;; after the opening one
                    (inline-element-node (eq (- close-tag-start-line open-tag-end-line) 1)))
          ;; Check that the element has no children others than open and close tag
          (eq (treesit-node-child-count element-node) 2))))))

(defun jtsx-electric-open-newline-between-jsx-element-tags-psif ()
  "Honor `jtsx-enable-electric-open-newline-between-jsx-element-tags'.
Member of `post-self-insert-hook'."
  (when (and (eq major-mode 'tsx-ts-mode)
             (eq last-command-event ?\n)
             (jtsx-jsx-context-p)
             (jtsx-inside-empty-inline-jsx-element-p))
    (save-excursion (newline-and-indent))))

;; Add hook for electric new line
(add-hook 'post-self-insert-hook #'jtsx-electric-open-newline-between-jsx-element-tags-psif nil)

(defconst jtsx-jsx-ts-root-keys '("jsx_element"
                                  "jsx_self_closing_element"
                                  "jsx_expression"
                                  "jsx_text"))

(defun jtsx-goto-line (line)
  "Go to the beginning of LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun jtsx-bolc-at-p (pos)
  "Check if POS is at the beginning of the line content."
  (eq pos (save-excursion (goto-char pos)
                          (back-to-indentation)
                          (point))))

(defun jtsx-eol-at-p (pos)
  "Check if POS is at the end of the line."
  (eq pos (save-excursion (goto-char pos)
                          (pos-eol))))

(defun jtsx-region-to-wrap ()
  "Return the expected region to be wrapped as a plist.
Keys are `:start' and `:end'."
  (let* ((region (if (region-active-p) (jtsx-trimmed-region) `(:start ,(point) :end ,(point))))
         (start-pos (plist-get region :start))
         (end-pos (plist-get region :end))
         (start-element (jtsx-enclosing-jsx-node (jtsx-treesit-node-at start-pos)
                                                 jtsx-jsx-ts-root-keys nil t))
         (end-element (if (region-active-p)
                          (jtsx-enclosing-jsx-node (jtsx-treesit-node-at end-pos)
                                                   jtsx-jsx-ts-root-keys nil t)
                        start-element))
         (start-element-type (treesit-node-type start-element))
         (end-element-type (treesit-node-type end-element)))
    (cl-assert (and start-element end-element))
    (if (and
         (region-active-p)
         (equal start-element-type "jsx_text")
         (equal end-element-type "jsx_text"))
        ;; Handle specific case: selection inside a text node (eg to wrap a text with `strong'
        ;; tags)
        `(:start ,start-pos :end ,end-pos)
      ;; General case: use treesit tree to get or adjust the expected region to be wrapped
      `(:start ,(treesit-node-start start-element) :end ,(treesit-node-end end-element)))))

(defun jtsx-inline-content-p (start-pos end-pos)
  "Return t if the content between START-POS and END-POS is inline.
The content is considered inline if there are some none whitespaces before
or after it."
  (or (not (jtsx-bolc-at-p start-pos)) (not (jtsx-eol-at-p end-pos))))

(defun jtsx-wrap-in-jsx-element (element-name)
  "Wrap JSX nodes in a JSX element.
Nodes are selected by a region if there is an active one.  Else the node at
 point is used.
ELEMENT-NAME is the name of the new wrapping element."
  (interactive "sJSX element name: ")
  (if (jtsx-jsx-context-p)
      (let* ((region-to-wrap (jtsx-region-to-wrap))
             (start-pos (plist-get region-to-wrap :start))
             (end-pos (plist-get region-to-wrap :end))
             (new-cursor-pos nil)
             (inline-element (jtsx-inline-content-p start-pos end-pos))
             (opening-line (line-number-at-pos start-pos))
             (closing-line (+ (line-number-at-pos end-pos)
                              (if inline-element 0 1))) ; +1 for insertion if not inline
             (opening-tag (format "<%s>" element-name))
             (closing-tag (format "</%s>" element-name)))
        (save-excursion
          (if inline-element (goto-char end-pos) (jtsx-goto-line closing-line))
          (insert closing-tag)
          (if (not inline-element) (newline))
          (if inline-element (goto-char start-pos) (jtsx-goto-line opening-line))
          (insert opening-tag)
          (setq new-cursor-pos (1- (point)))
          (if (not inline-element) (newline)))
        ;; Let the cursor ready to add attributes in the wrapping element
        (goto-char new-cursor-pos)
        ;; Finally indent modified region
        (indent-region (save-excursion (jtsx-goto-line opening-line) (pos-bol))
                       (save-excursion (jtsx-goto-line (+ closing-line (if inline-element 0 1)))
                                       (pos-eol))))
    (message "Not inside jsx context")))
