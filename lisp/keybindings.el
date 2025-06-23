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

  "w" '(:ignore t :which-key "windows")
  "ww" '(other-window :which-key "other window")
  "wv" '(split-window-right :which-key "vertical split")
  "ws" '(split-window-below :which-key "horizontal split")
  "wd" '(delete-window :which-key "close window")

  "h" '(:ignore t :which-key "help")
  "h d" '(:ignore t :which-key "describe")
  "h d f" '(describe-function :which-key "function")
  "h d v" '(describe-variable :which-key "variable")
  "h d c" '(describe-command :which-key "command")
  "h d k" '(describe-key :which-key "key")

  "c" '(:ignore t :which-key "code")
  "c d" '(lsp-ui-peek-find-definitions :which-key "definition")
  "c r" '(lsp-ui-peek-find-references :which-key "references")
  "c c" '(lsp-rename :which-key "rename")
 )
(provide 'keybindings)
