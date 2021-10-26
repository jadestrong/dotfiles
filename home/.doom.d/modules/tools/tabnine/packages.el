;; -*- no-byte-compile: t; -*-
;;; tools/tabnine/packages.el

(when (featurep! :completion company)
  (package! company-tabnine :recipe (:host github :repo "TommyX12/company-tabnine")))
