;;; mk-keymaps.el --- Configure prefix keymaps -*- lexical-binding: t -*-

(defvar-keymap mk-prefix-window-map
  :doc "Prefix keymap for windows."
  :name "Window"
  "u" #'winner-undo
  "r" #'winner-redo
  "b" #'balance-windows-area
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "^" #'tear-off-window
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "H" #'windmove-swap-states-left
  "J" #'windmove-swap-states-down
  "K" #'windmove-swap-states-up
  "L" #'windmove-swap-states-right)

(defvar-keymap mk-prefix-buffer-map
  :doc "Prefix keymap for buffers."
  :name "Buffer"
  "b" #'switch-to-buffer
  "c" #'clone-indirect-buffer-other-window
  "f" #'fit-window-to-buffer
  "g" #'revert-buffer-quick
  "n" #'next-buffer
  "p" #'previous-buffer)

(defvar-keymap mk-prefix-mode-map
  :doc "Prefix keymap for minor mode toggles."
  :name "Toggle"
  "f" #'flymake-mode
  "h" #'hl-line-mode
  ;;  "l" #'logos-focus-mode
  "m" #'menu-bar-mode
  "n" #'display-line-numbers-mode
  "t" #'toggle-truncate-lines
  "p" #'spacious-padding-mode) ; "padding" mnemonic)

(defvar-keymap mk-prefix-file-map
  :doc "Prefix keymaps for files."
  :name "File"
  "r" #'recentf
  "f" #'find-file
  "F" #'find-file-other-window
  "b" #'bookmark-jump
  "d" #'dired
  "l" #'find-library
  "m" #'man)


(defvar-keymap mk-prefix-map
  :doc "Prefix keymap with multiple subkeymaps."
  :name "Prefix"
  "0" #'delete-window
  "1" #'delete-other-windows
  "!" #'delete-other-windows-vertically
  "2" #'split-window-below
  "@" #'split-root-window-below
  "3" #'split-window-right
  "#" #'split-root-window-right
  "o" #'other-window
  "Q" #'save-buffers-kill-emacs
  "b" mk-prefix-buffer-map
  "f" mk-prefix-file-map
  "g" #'magit-status
  "h" help-map
  "j" #'dired-jump
  "m" mk-prefix-mode-map
  "n" narrow-map
  "p" project-prefix-map
  "r" ctl-x-r-map
  "u" #'universal-argument
  "v" vc-prefix-map
  "w" mk-prefix-window-map)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements mk-prefix-map
    "w" `("Window" . ,mk-prefix-window-map)
    "b" `("Buffer" . ,mk-prefix-buffer-map)
    "m" `("Modes" . ,mk-prefix-mode-map)
    "h" `("Help" . ,help-map)
    "p" `("Project" . ,project-prefix-map)
    "f" `("Files" . ,mk-prefix-file-map)))

(mk-emacs-keybind global-map
  "<insert>" nil
  "C-x C-z" nil
  "C-x C-c" nil
  "<f2>" nil
  "M-SPC" nil
  "C-x C-c C-c" #'save-buffers-kill-emacs
  "<f2>" mk-prefix-map
  "M-SPC" mk-prefix-map
  "M-o" #'delete-blank-lines)

(provide 'mk-keymaps)
