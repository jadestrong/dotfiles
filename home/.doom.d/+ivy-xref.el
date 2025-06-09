;;; ../dotfiles/home/.doom.d/+ivy-xref.el -*- lexical-binding: t; -*-

(defadvice! +ivy-xref-make-collection (xrefs)
  :override #'ivy-xref-make-collection
  (let ((collection nil))
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let* ((line (xref-location-line location))
               (file (xref-location-group location))
               (candidate
                (counsel--normalize-grep-match
                 (concat
                  (propertize
                   (concat
                    (if ivy-xref-use-file-path
                        file
                      (file-name-nondirectory file))
                    (if (integerp line)
                        (format ":%d:" line)
                      ":"))
                   'face 'compilation-info)
                  (progn
                    (when ivy-xref-remove-text-properties
                      (set-text-properties 0 (length summary) nil summary))
                    summary)))))
          (push `(,candidate . ,location) collection))))
    (nreverse collection)))

(defadvice! +ivy-xref-show-xrefs (fetcher alist)
  :override #'ivy-xref-show-xrefs
  (let* ((xrefs (if (functionp fetcher)
                    ;; Emacs 27
                    (or (assoc-default 'fetched-xrefs alist)
                        (funcall fetcher))
                  fetcher))
         (buffer (xref--show-xref-buffer fetcher alist)))
    (quit-window)
    (let ((orig-buf (current-buffer))
          (orig-pos (point))
          done)
      (ivy-read "xref: " (ivy-xref-make-collection xrefs)
                :require-match t
                :action (lambda (candidate)
                          (let ((candidate (or candidate
                                               (with-current-buffer (ivy--find-occur-buffer)
                                                 (get-text-property (line-beginning-position)
                                                                    'ivy-xref-candidate)))))
                            (setq done (eq 'ivy-done this-command))
                            (condition-case err
                                (let* ((marker (xref-location-marker (cdr candidate)))
                                       (buf (marker-buffer marker)))
                                  (with-current-buffer buffer
                                    (select-window
                                     ;; function signature changed in
                                     ;; 2a973edeacefcabb9fd8024188b7e167f0f9a9b6
                                     (if (version< emacs-version "26.0.90")
                                         (xref--show-pos-in-buf marker buf t)
                                       (xref--show-pos-in-buf marker buf)))))
                              (user-error (message (error-message-string err))))))
                :unwind (lambda ()
                          (unless done
                            (switch-to-buffer orig-buf)
                            (goto-char orig-pos)))
                :caller 'ivy-xref-show-xrefs))
    ;; honor the contract of xref--show-xref-buffer by returning its original
    ;; return value
    buffer))

(defun ivy-xref--occur-insert-lines (cands)
  "Insert CANDS into `ivy-occur' buffer."
  (font-lock-mode -1)
  (dolist (cand cands)
    (let ((cand-list (assoc cand (ivy-state-collection ivy-last) #'string=)))
      (setq cand
            (if (string-match "\\`\\(.*:[0-9]+:\\)\\(.*\\)\\'" cand)
                (let ((file-and-line (match-string 1 cand))
                      (grep-line (match-string 2 cand)))
                  (concat
                   (propertize file-and-line 'face 'ivy-grep-info)
                   (ivy--highlight-fuzzy grep-line)))
              (ivy--highlight-fuzzy (copy-sequence cand))))
      (add-text-properties
       0 (length cand)
       `(mouse-face
         highlight
         help-echo "mouse-1: call ivy-action"
         ivy-xref-candidate ,cand-list)
       cand)
      (insert (if (ivy--starts-with-dotslash cand) "" "    ")
              cand ?\n))))

(defun ivy-xref--occur-make-buffer (cands)
  (let ((inhibit-read-only t))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy-xref--occur-insert-lines cands)
    (goto-char (point-min))
    (forward-line 4)))

(defun ivy-xref-show-xrefs-occur (&optional cands)
  "Generate a custom occur buffer for `ivy-xref-show-xrefs'"
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (ivy-xref--occur-make-buffer cands))

(ivy-configure 'ivy-xref-show-xrefs
  :occur #'ivy-xref-show-xrefs-occur)
