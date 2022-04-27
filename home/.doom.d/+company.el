;;; ../dotfiles/home/.doom.d/+company.el -*- lexical-binding: t; -*-

;; (use-package! company-tabnine
;;   :when (featurep! :completion company)
;;   :config
;;   (defun company//sort-by-tabnine (candidates)
;;     (if (or (functionp company-backend)
;;             (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
;;         candidates
;;       (let ((candidates-table (make-hash-table :test #'equal))
;;             candidates-1
;;             candidates-2)
;;         (dolist (candidate candidates)
;;           (if (eq (get-text-property 0 'company-backend candidate)
;;                   'company-tabnine)
;;               (unless (gethash candidate candidates-table)
;;                 (push candidate candidates-2))
;;             (push candidate candidates-1)
;;             (puthash candidate t candidates-table)))
;;         (setq candidates-1 (nreverse candidates-1))
;;         (setq candidates-2 (nreverse candidates-2))
;;         (nconc (seq-take candidates-1 2)
;;                (seq-take candidates-2 2)
;;                (seq-drop candidates-1 2)
;;                (seq-drop candidates-2 2)))))
;;   (add-to-list 'company-transformers 'company//sort-by-tabnine t)
;;   ;; The free version of TabNine is good enough,
;;   ;; and below code is recommended that TabNine not always
;;   ;; prompt me to purchase a paid version in a large project.
;;   ;; 禁止tabnine提示升级付费版本
;;   (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
;;       (let ((company-message-func (ad-get-arg 0)))
;;         (when (and company-message-func
;;                    (stringp (funcall company-message-func)))
;;           (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
;;             ad-do-it))))
;;   ;;将tabnine添加到backends
;;   (add-to-list 'company-backends 'company-tabnine))



;; (defun company/remove-duplicate-cands (candidates)
;;   ;; (dolist (candidate candidates)
;;   ;;   (setq backend-property (get-text-property 0 'company-backend candidate))
;;   ;;   (setq completion-item (get-text-property 0 'lsp-completion-item candidate))
;;   ;;   (message "%s" backend-property)
;;   ;;   (message "%s" completion-item))
;;   ;; candidates
;;   (let ((newseq))
;;     (dolist (candidate candidates)
;;       (setq backend-property (get-text-property 0 'company-backend candidate))
;;       (setq completion-item (get-text-property 0 'lsp-completion-item candidate))
;;       (if (not (member candidate newseq))
;;           (push candidate newseq)
;;         (if completion-item
;;             (progn
;;               (setq newseq (delq candidate candidates))
;;               (push candidate candidates)
;;               )
;;           )
;;         )
;;       )
;;     (nreverse newseq))
;;   )
