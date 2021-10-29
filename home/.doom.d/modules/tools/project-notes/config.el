;;; tools/project-notes/config.el -*- lexical-binding: t; -*-

;; Create a TODO list based on TODO items in a project's `.project-notes.org' file. The
;; `org-agenda-files' variable is temporarily set the only the project notes file and then reverted
;; back to its previous value upon closing the TODO list buffer.

(defun iensu-add-to-list (list &rest items)
  "Add multiple items to a list."
  (dolist (item items)
    (add-to-list list item)))

(defvar iensu--project-agenda-buffer-name "*Project Agenda*")

(defun iensu--org-capture-project-notes-file ()
  (concat (locate-dominating-file (buffer-file-name) ".git") ".project-notes.org"))

(defun iensu/create-project-notes-file ()
  "Creates a note file somewhere in `org-directory' and links it to the current directory as `.project-notes.org.'"
  (interactive)
  (let* ((versioned-dir (locate-dominating-file (buffer-file-name)
                                                ".git"))
         (project-dir (expand-file-name (or versioned-dir
                                           (file-name-directory (buffer-file-name)))))
         (project-name (car (last (remove-if (lambda (x) (string-equal ""
                                                                  x))
                                             (split-string project-dir
                                                           "/")))))
         (notes-link (concat project-dir ".project-notes.org"))
         (notes-file-name (concat project-name ".org"))
         (notes-dir (expand-file-name (read-directory-name (format "Where to save %s? "
                                                                   notes-file-name)
                                                           (concat (file-name-as-directory org-directory)
                                                                   "projects"))))
         (notes-file (concat notes-dir notes-file-name)))
    (make-empty-file notes-file)
    (make-symbolic-link notes-file notes-link)
    (find-file notes-link)
    (iensu/refresh-agenda-files)))

(defun iensu/refresh-agenda-files ()
  (interactive)
  (setq-default org-agenda-files
                (append (directory-files org-directory
                                         :full-path
                                         "\\.org$")
                        (directory-files-recursively (concat org-directory "/projects")
                                                     "\\.org$")
                        (directory-files-recursively (concat org-directory "/calendars")
                                                     "\\.org$"))))

(defun iensu/project-todo-list ()
  (interactive)
  (let ((project-notes-file (expand-file-name ".project-notes.org"
                                              (project-root (project-current)))))
    (if (file-exists-p project-notes-file)
        (progn
          (setq org-agenda-files `(,project-notes-file))
          (org-todo-list)
          (rename-buffer iensu--project-agenda-buffer-name 'unique))
      (message "Could not locate any project notes file"))))

(defun iensu/reset-org-agenda-files ()
  (interactive)
  (when (string-equal iensu--project-agenda-buffer-name
                      (buffer-name (current-buffer)))
    (setq org-agenda-files iensu-org-agenda-files)))

(defun iensu/open-project-org-file ()
  (interactive)
  (find-file (iensu--org-capture-project-notes-file)))

;; Reset org-agenda-files when the project TODO list buffer is closed
(add-hook 'kill-buffer-hook #'iensu/reset-org-agenda-files)

;; Add some org-capture templates for project notes.
(iensu-add-to-list 'org-capture-templates
                   `("m" "Project note" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "%?")
                     :empty-lines 1)

                   `("l" "Project note with link" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "Link: %a\n\n"
                              "%?")
                     :empty-lines 1)

                   `("N" "Project note with link + code quote" entry (file+headline iensu--org-capture-project-notes-file "Notes")
                     ,(concat "* %^{Title}\n"
                              "%U\n\n"
                              "Link: %a\n\n"
                              "#+begin_src %^{Language}\n"
                              "%i\n"
                              "#+end_src"
                              "\n\n"
                              "%?")
                     :empty-lines 1))
