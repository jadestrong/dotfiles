;;; editor/meow/config.el -*- lexical-binding: t; -*-

;; Setup Functions

;; Leader Key
(defun meow/setup-leader ()
 (map!
  :g "C-c C-d" #'+lookup/definition
  :g "M-n" #'meow-page-down
  :g "M-p" #'meow-page-up
  :g "M-i" #'switch-to-prev-buffer
  :g "M-o" #'switch-to-next-buffer
  :leader
  "?" #'meow-cheatsheet
  "/" #'meow-keypad-describe-key
  "1" #'meow-digit-argument
  "2" #'meow-digit-argument
  "3" #'meow-digit-argument
  "4" #'meow-digit-argument
  "5" #'meow-digit-argument
  "6" #'meow-digit-argument
  "7" #'meow-digit-argument
  "8" #'meow-digit-argument
  "9" #'meow-digit-argument
  "0" #'meow-digit-argument
  "p r" #'projectile-recentf
  "p p" #'counsel-projectile-switch-project
  "p f" #'+ivy/projectile-find-file
  "g g" #'magit-status
  "w v" #'split-window-right
  "w s" #'split-window-below
  "c" #'meow-keypad-start
  "x" #'meow-keypad-start))

;; Keypad
(defun meow/setup-keypad ()
 (map! :map meow-keypad-state-keymap
  "?" #'meow-cheatsheet
  "/" #'meow-keypad-describe-key
  "1" #'meow-digit-argument
  "2" #'meow-digit-argument
  "3" #'meow-digit-argument
  "4" #'meow-digit-argument
  "5" #'meow-digit-argument
  "6" #'meow-digit-argument
  "7" #'meow-digit-argument
  "8" #'meow-digit-argument
  "9" #'meow-digit-argument
  "0" #'meow-digit-argument
  "h" #'help-command))

;; applies to all layouts (except dvp)
(defun meow/setup ()
  (map! :map meow-normal-state-keymap
   "0" #'meow-expand-0
   "1" #'meow-expand-1
   "2" #'meow-expand-2
   "3" #'meow-expand-3
   "4" #'meow-expand-4
   "5" #'meow-expand-5
   "6" #'meow-expand-6
   "7" #'meow-expand-7
   "8" #'meow-expand-8
   "9" #'meow-expand-9
   "-" #'negative-argument
   ";" #'meow-reverse
   "," #'meow-inner-of-thing
   "." #'meow-bounds-of-thing
   "'" #'repeat
   "<escape>" #'ignore))

;; Qwerty
(defun meow/setup-qwerty ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow/setup)
  (when (modulep! :editor meow +override)
   (meow-motion-overwrite-define-key
    '("j" . meow-next)
    '("k" . meow-prev))
   (when (modulep! :editor meow +leader)
     (meow-motion-overwrite-define-key
      '("\\ j" . "H-j")
      '("\\ k" . "H-k")))
   (meow-leader-define-key
    ;; SPC j/k will run the original command in MOTION state.
    '("j" . "H-j")
    '("k" . "H-k")))
  (map! :map meow-normal-state-keymap
   "[" #'meow-beginning-of-thing
   "]" #'meow-end-of-thing
   "a" #'meow-append
   "A" #'meow-open-below
   "b" #'meow-back-word
   "B" #'meow-back-symbol
   "c" #'meow-change
   "d" #'meow-delete
   "D" #'meow-backward-delete
   "e" #'meow-next-word
   "E" #'meow-next-symbol
   "f" #'meow-find
   "g" #'meow-cancel-selection
   "G" #'meow-grab
   "h" #'meow-left
   "H" #'meow-left-expand
   "i" #'meow-insert
   "I" #'meow-open-above
   "j" #'meow-next
   "J" #'meow-next-expand
   "k" #'meow-prev
   "K" #'meow-prev-expand
   "l" #'meow-right
   "L" #'meow-right-expand
   "m" #'meow-join
   "n" #'meow-search
   "o" #'meow-block
   "O" #'meow-to-block
   "p" #'meow-yank
   "q" #'meow-quit
   "Q" #'meow-goto-line
   "r" #'meow-replace
   "R" #'meow-swap-grab
   "s" #'meow-kill
   "t" #'meow-till
   "u" #'meow-undo
   "U" #'meow-undo-in-selection
   "v" #'meow-visit
   "w" #'meow-mark-word
   "W" #'meow-mark-symbol
   "x" #'meow-line
   "X" #'meow-goto-line
   "y" #'meow-save
   "Y" #'meow-sync-grab
   "@" #'meow-pop-selection
   "z z" #'recenter))

(use-package! meow
  :hook (doom-after-modules-config . meow-global-mode)
  :demand t
  :config
  (setq meow-use-cursor-position-hack t)
  (meow/setup-qwerty)
  (cond
   ((modulep! +leader)
    (map! :map meow-normal-state-keymap
      doom-leader-key doom-leader-map)
    (map! :map meow-motion-state-keymap
     doom-leader-key doom-leader-map)
    (map! :map meow-beacon-state-keymap
     doom-leader-key nil)
    (meow/setup-leader))
   (t (meow/setup-keypad)))
  (map! :map meow-keymap [remap describe-key] #'helpful-key))
