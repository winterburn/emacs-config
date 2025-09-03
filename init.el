(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar

(setq visible-bell t) ; Set up the visible bell

;; Make esc quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; ; Add init .el to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)) 

;; Initialize package sources

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
	        term-mode-hook
	        eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "IN PROGRESS(i!)" "BLOCKED(b@/!)" "|" "DONE(d)" "CANCELED(c@)"))
	org-log-done 'time
        org-default-notes-file (concat org-directory "/notes.org")
	org-clock-persist 'history
	org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/tasks.org" "Tasks")
	   "* TODO %?\n %U\n %a")
	  ("p" "Python Note"
	   entry (file+headline "~/org/notes.org" "Python Notes")
	   "* %^{heading}\n:PROPERTIES:\n:Captured: %U\n:Source: [[%F::%(with-current-buffer (org-capture-get :original-buffer) (number-to-string (line-number-at-pos)))][%f]]\n:END:\n\n#+BEGIN_SRC python \n%i\n#+END_SRC\n\n%?")))
  (org-clock-persistence-insinuate))



(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;; (use-package ivy
;;   :config
;;   (ivy-mode 1))
;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1))
;; (use-package counsel
;;   :bind (("M-x" . counsel-M-x)
;; 	 ("C-x C-f" . counsel-find-file)))
;; (use-package counsel-projectile
;;   :init
;;   (counsel-projectile-mode 1)
;;   :config
;;   (my/leader-keys
;;     "p" 'projectile-command-map))

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-prescient
  :init
  (vertico-prescient-mode)
  (prescient-persist-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(defun my/doom-modeline-update-env ()
  "Set the modeline to show current the name of the current uv .venv."
  (if (derived-mode-p 'python-mode)
      (let ((venv (getenv "VIRTUAL_ENV")))
	(setq doom-modeline-env--version
	      (when venv
		(file-name-nondirectory (directory-file-name (file-name-parent-directory (getenv "VIRTUAL_ENV"))))))
	       ))
  )

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-tokyo-night t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-enable-python t)
  (setq doom-modeline-after-update-env-hook 'my/doom-modeline-update-env))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))



(use-package helpful
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :bind
  (:map evil-normal-state-map
	("C-u" . evil-scroll-up))
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package general
  :after evil
  :init
  (setq general-override-state '(insert
				 emacs
				 hybrid
				 normal
				 visual
				 motion
				 operator
				 replace))
  :config
  (general-evil-setup t))
(require 'keybindings)

(use-package magit
  :config
  (custom-set-faces
   '(magit-blame-heading ((t (:background "SlateBlue4")))))
  (my/leader-keys
    "g" '(:ignore t :which-key "git")
    "g g" '(magit-status :which-key "status")
    "g b" '(magit-blame-addition :which-key "blame")
    "g f" '(magit-fetch :which-key "fetch")
    "g F" '(magit-fetch-all :which-key "fetch-all")))

(use-package git-gutter
  :hook ((prog-mode . git-gutter-mode)
	 (magit-post-refresh . git-gutter:update-all-windows)))
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package lsp-mode
  :hook (
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-format-buffer-on-save t)
  (lsp-headerline-breadcrumb-mode 1)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind
  ([remap evil-jump-backward] . lsp-ui-peek-jump-backward)
  ([remap evil-jump-forward] . lsp-ui-peek-jump-forward))

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright"))

(use-package yasnippet
  :hook
  (after-init . yas-reload-all))
(use-package yasnippet-snippets)
(use-package company
  :bind(:map company-active-map
	     ("<return>" . nil)
	     ("RET" . nil)
	     ("TAB" . company-complete-selection)
	     ("<tab>" . company-complete-selection))
  :hook (after-init . global-company-mode))

(use-package company-prescient
  :init
  (company-prescient-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package dap-mode
  :init
  (setopt dap-auto-configure-mode t)
  :after lsp-mode
  :commands dab-debug)


(defmacro company-backend-for-hook (hook backends)
  `(add-hook ,hook (lambda ()
		     (set (make-local-variable 'company-backends)
			  ,backends))))

(defun my/setup-python-environment ()
  "Setup a python development environment in the current buffer."
  ;; Update the environment
  (envrc--update)
  (yas-minor-mode 1)
  ;; setup active backends for python mode
  (company-backend-for-hook 'lsp-completion-mode-hook
			    '((company-capf :with company-yasnippet)
			      company-dabbrev-code))
  (require 'lsp-pyright)
  (lsp-deferred)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (dap-mode 1))

(add-hook 'python-mode-hook #'my/setup-python-environment)

(setopt company-backends '((company-capf company-dabbrev-code)))

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (let ((paths '(("~/repos/" . 1)
		 ("/mnt/d/Koodia/" .1))))
    (dolist (path-depth paths)
      (let ((dir (car path-depth))
	    (depth (cdr path-depth)))
	(when (file-directory-p dir)
	  (add-to-list 'projectile-project-search-path (cons dir depth))))))
  (projectile-mode +1))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  :config
  (my/leader-keys
    "s s" '(consult-line :which-key "search in buffer.")
    "s S" '(consult-line-multi :which-key "search in project.")))
(use-package consult-lsp)

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("Python" (ruff "format")))))

(use-package avy
  :config
  (my/leader-keys
    "s f" '(avy-goto-char-timer :which-key "find in window")))

(use-package hydra)

(defhydra hydra-zoom ()
  "zoom"
  ("k" text-scale-increase "in")
  ("j" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(my/leader-keys
  "u" '(:ignore t :which-key "toggles")
  "u z" '(hydra-zoom/body :which-key "zoom"))

(use-package rg)

(use-package indent-bars
  :hook ((python-mode) . indent-bars-mode))

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package flycheck-projectile)


(use-package pespective
  

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(flycheck-projectile flycheck company-prescient vertico-prescient indent-bars rg org-bullets vertico marginalia orderless vertigo format-all dap-python dap-mode consult-lsp git-gutter-fringe git-gutter projectile evil-commentary yasnippet-snippets yasnippet evil-surround lsp-ivy lsp-mode company envrc magit evil-collection general evil helpful counsel ivy-rich which-key rainbow-delimiters doom-themes nerd-icons doom-modeline ivy))
 '(safe-local-variable-values '((checkdoc-allow-quoting-nil-and-t . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-blame-heading ((t (:background "SlateBlue4")))))
