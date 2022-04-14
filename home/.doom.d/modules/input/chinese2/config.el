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
  ;; (rime-emacs-module-header-root "/nix/store/h6ahwbvap9a3pxhq5bdkhkxhpdj6x07f-emacsGccDarwin/include")
  (rime-librime-root "~/.doom.d/librime/dist")
  (rime-inline-ascii-trigger 'shift-l)
  (rime-show-candidate 'posframe)
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
  (setq-default rime-disable-predicates
                '(rime-predicate-evil-mode-p
                  rime-predicate-after-alphabet-char-p
                  rime-predicate-prog-in-code-p
                  rime-predicate-in-code-string-p))
  (setq-default rime-inline-predicates
                '(rime-predicate-space-after-cc-p)))
