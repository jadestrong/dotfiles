;;; tools/tabnine/config.el -*- lexical-binding: t; -*-
(defcustom company-tabnine-capf-threshold 60
  "the tabnine threshold for sorting two different backend"
  :group 'company-tabnine-capf
  :type 'integer)

;; (setq company-tabnine-binaries-folder "~/.emacs.d/.cache/tabnine")
(setq company-tabnine-binaries-folder "~/.TabNine")

(defun company-tabnine-capf--annotation (candidate)
  "read annotation from candidate
company-tabnine's annotation is stored in text properties, so smart!
so if annotation cannot read from properties, just put into company-capf"
  (let ((annotation (get-text-property 0 'annotation candidate)))
    (if annotation
        annotation
      (company-capf--annotation candidate))))

(defun company-tabnine-capf--post-completion (candidate)
  "call post completion and return nil"
  (if (get-text-property 0 'old_suffix candidate)
      (company-tabnine--post-completion candidate)
    (company--capf-post-completion candidate)))

(defun company-tabnine-capf--mix-candidates (&optional tabnine-result capf-result)
  (cond
     ((eq (length tabnine-result) 0) capf-result)
     ((eq (length capf-result) 0) tabnine-result)
     ((> (company-tabnine-capf--extract-tabnine-confidence (car tabnine-result)) company-tabnine-capf-threshold)
      `(,(car tabnine-result) ,@(company-tabnine-capf--mix-candidates (cdr tabnine-result) capf-result)))
     (t `(,@capf-result ,@tabnine-result))))

(defun company-tabnine-capf--candidates (prefix)
  "combine and sort the company-tabnine and the company-capf results.
if company-tabnine's confidence is greater then `company-tabnine-capf-threshold',
tabnine's candidate have greater position then others."
  (let ((tabnine-result (company-tabnine--candidates prefix))
        (capf-result (company-capf--candidates prefix)))
    (company-tabnine-capf--mix-candidates tabnine-result capf-result)))

(defun company-tabnine-capf--extract-tabnine-confidence (candidate)
  "extract integer from company-tabnine's candidate"
  (string-to-number (get-text-property 0 'annotation candidate)))

(defun company-tabnine-capf--meta (candidate)
  "return meta data for candidate"
  (let ((tabnine-meta (company-tabnine--meta candidate)))
    (if tabnine-meta
        tabnine-meta
      (company-capf 'meta candidate))))

(defun company-lsp--candidate-item (candidate)
  "Retrieve the CompletionItem hashtable associated with CANDIDATE.
CANDIDATE is a string returned by `company-lsp--make-candidate'."
  (plist-get (text-properties-at 0 candidate) 'lsp-completion-item))

(defun company-lsp--resolve-candidate (candidate &rest props)
  "Resolve a completion candidate to fill some properties.
CANDIDATE is a string returned by `company-lsp--make-candidate'.
PROPS are strings of property names of CompletionItem hashtable
to be resolved.
The completionItem/resolve request will only be sent to the
server if the candidate has not been resolved before, and at lest
one of the PROPS of the CompletionItem is missing.
Returns CANDIDATE with the resolved CompletionItem."
  (unless (plist-get (text-properties-at 0 candidate) 'company-lsp-resolved)
    (let ((item (company-lsp--candidate-item candidate)))
      (when (seq-some (lambda (prop)
                        (null (gethash prop item)))
                      props)
        (let ((resolved-item (lsp-completion--resolve item))
              (len (length candidate)))
          (put-text-property 0 len
                             'lsp-completion-item resolved-item
                             candidate)
          (put-text-property 0 len
                             'company-lsp-resolved t
                             candidate)))))
  candidate)

(defvar company-lsp-document-language
  '(("typescriptreact" . "tsx")
    ("javascriptreact" . "jsx")
    ("javascript" . "typescript"))
  "Get the document's languageId from textDocument/resolve")

(defun company-lsp--get-language (config language)
  "get the config value for document language"
  (if (listp config)
      (if-let (language-config (assoc language config))
          (cdr language-config)
        (alist-get t config))
    config))

(defun company-lsp--documentation (candidate)
  "Get the documentation from the item in the CANDIDATE.
The documentation can be either string or MarkupContent. This method
will return markdown string if it is MarkupContent, original string
otherwise. If the documentation is not present, it will return nil
which company can handle."
  (let* ((resolved-candidate (company-lsp--resolve-candidate candidate "documentation"))
         (item (company-lsp--candidate-item resolved-candidate))
         (documentation (gethash "documentation" item))
         (detail (make-hash-table :test 'equal)))
    (puthash "language" (or (company-lsp--get-language company-lsp-document-language (lsp-buffer-language)) (lsp-buffer-language)) detail)
    (puthash "value" (gethash "detail" item) detail)
    (setq contents (list detail documentation))
    (string-join
     (seq-map
      #'lsp--render-element
      contents)
     (if (bound-and-true-p page-break-lines-mode)
         "\n\n"
       "\n\n"))))

;;;###autoload
(defun company-tabnine-capf (command &rest args)
  "a company backend for combine tabnine and capf"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tabnine-capf))
    (prefix
     (apply 'company-tabnine `(,command ,@args))
     (apply 'company-capf `(,command ,@args)))
    (meta (company-tabnine-capf--meta (car args)))
    (annotation (company-tabnine-capf--annotation (car args)))
    (sorted t)
    (no-cache t)
    (kind (apply 'company-capf `(,command ,@args)))
    (post-completion (company-tabnine-capf--post-completion (car args)))
    (candidates (company-tabnine-capf--candidates (car args)))
    (doc-buffer (company-doc-buffer (company-lsp--documentation (car args))))
    (quickhelp-string (company-lsp--documentation (car args)))))

(defun toggle-company-tabnine-capf ()
  "toggle company-tabnine-capf backend"
  (interactive)
  (when (not (file-exists-p company-tabnine-binaries-folder))
    (company-tabnine-install-binary))
  (if (memq 'company-tabnine-capf company-backends)
      (progn
        (setq company-backends (remove 'company-tabnine-capf company-backends))
        (message "company-tabnine-capf disabled"))
    (add-to-list 'company-backends 'company-tabnine-capf)
    (message "company-tabnine-capf enabled!")))

;; (defadvice! ++lsp-init-completion-backends-h ()
;;   :override +lsp-init-company-backends-h
;;   (when lsp-completion-mode
;;    (if (memq 'company-tabnine-capf company-backends)
;;       (progn
;;         (setq company-backends (remove 'company-tabnine-capf company-backends))
;;         (message "company-tabnine-capf disabled"))
;;     (add-to-list 'company-backends 'company-tabnine-capf)
;;     (message "company-tabnine-capf enabled!"))))
