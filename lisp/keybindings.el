;; keybinding definitions


(general-create-definer my/leader-keys
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(my/leader-keys
  "f" '(:ignore t :which-key "files")
  "ff" '(counsel-find-file :which-key "find file")
  "fs" '(save-buffer :which-key "save file")

  "b" '(:ignore t :which-key "buffers")
  "bb" '(counsel-switch-buffer :which-key "switch buffer")
  "bd" '(kill-this-buffer :which-key "kill buffer")

  "w" '(:ignore :which-key "windows")
  "ww" '(other-window :which-key "other window")
  "wv" '(split-window-right :which-key "vertical split")
  "ws" '(split-window-below :which-key "horizontal split")
  "wd" '(delete-window :which-key "close window")
)
(provide 'keybindings)
