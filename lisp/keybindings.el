;;; Package --- Summary keybinding definitions


;;; Code:
(general-create-definer my/leader-keys
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(my/leader-keys
  ":" '(execute-extended-command :which-key "execute commands")
  "SPC" '(projectile-find-file :which-key "find file in project")

  "f" '(:ignore t :which-key "files")
  "f f" '(find-file :which-key "find file")
  "f s" '(save-buffer :which-key "save file")
  "f c" '(copy-file :which-key "copy file")

  "b" '(:ignore t :which-key "buffers")
  "b b" '(consult-buffer :which-key "switch buffer")
  "b d" '(kill-this-buffer :which-key "kill buffer")

  "w" '(:ignore t :which-key "windows")
  "w w" '(other-window :which-key "other window")
  "w v" '(split-window-right :which-key "vertical split")
  "w s" '(split-window-below :which-key "horizontal split")
  "w d" '(delete-window :which-key "close window")

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
  "c x" '(flycheck-list-errors :which-key "buffer error list")
  "c X" '(flycheck-projectile-list-errors :which-key "project error list")
  "c n" '(flycheck-next-error :which-key "next error")
  "c p" '(flycheck-previous-error :which-key "previous error")
  ;; Debug things
  "c b" '(:ignore t :which-key "debug")
  "c b b" '(dap-breakpoint-toggle :which-key "toggle breakpoint")
  "c b B" '(dap-breakpoint-delete-all :which-key "remove all breakpoints")
  "c b n" '(dap-next :which-key "next")
  "c b i" '(dap-step-in :which-key "step in")
  "c b I" '(dap-step-out :which-key "step out")
  "c b d" '(dap-debug :which-key "start debug")
  "c b r" '(dap-debug-last :which-key "start debug (previous)")
  "c b c" '(dap-continue :which-key "continue")
  "c b x" '(dap-disconnect :which-key "stop debugging")

  "s" '(:ignore t :which-key "search")
  "s p" '(consult-ripgrep :which-key "search project")

  "o" '(:ignore t :which-key "org")
  "o a" '(org-agenda :which-key "agenda")
  "o c" '(org-capture :which-key "capture")
  "o k" '(org-todo :which-key "cycle todo")
  "o o" '(org-open-at-point :which-key "Open link at point")
  "o t" '(:ignore t :which-key "clock")
  "o t i" '(org-clock-in :which-key "clock in")
  "o t o" '(org-clock-out :which-key "clock out")
  "o t j" '(org-clock-goto :which-key "jump to current clock")

  "p" '(projectile-command-map :which-key "projects")

 )
(provide 'keybindings)
;;; keybindings.el ends here
