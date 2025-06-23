(setq inhibit-startup-message t)
(setq custom-safe-themes t)

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

(use-package ivy
  :config
  (ivy-mode 1))

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
  (load-theme 'doom-tokyo-night)
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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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
    (my/leader-keys
      "g" '(:ignore t :which-key "git")
      "g g" '(magit-status :which-key "status")
    )
  )

(use-package lsp-mode
  :hook (
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-format-buffer-on-save t)
  (lsp-headerline-breadcrumb-mode 1)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols)))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright"))

(use-package yasnippet
  :hook
  (after-init . yas-reload-all))
(use-package yasnippet-snippets)
(use-package company
  :hook (after-init . global-company-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode))

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
  (lsp-deferred))

(add-hook 'python-mode-hook #'my/setup-python-environment)

(setopt company-backends '((company-capf company-dabbrev-code)))

(use-package projectile
  :config
  (setq projectile-project-search-path '(("~/repos/" . 1) ( "/mnt/d/Koodia/" . 1))))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode 1)
  :config
  (my/leader-keys
    "p" 'projectile-command-map))

(use-package centaur-tabs
  :config
  (centaur-tabs-mode t)
  (setq centaur-tabs-style "wave"
        centaur-tabs-set-icons t
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-cycle-scope 'tabs
	centaur-tabs-height 32)
  (centaur-tabs-group-by-projectile-project)
  :bind
  (:map evil-normal-state-map
	("g t" . centaur-tabs-forward)
	("g T" . centaur-tabs-backward))
  :config
  (my/leader-keys
    "t" '(:ignore t :switch-key "tabs")
    "t g" '(centaur-tabs-counsel-switch-group :switch-key "select group")
    "t t" '(centaur-tabs-forward :switch-key "tab forward")
    "t T" '(centaur-tabs-backward :switch-key "tab backward")
    "t u" '(centaur-tabs-backward-group :switch-key "group backward")
    "t i" '(centaur-tabs-forward-group :switch-key "group forward")
    "t k" '(centaur-tabs-kill-other-buffers-in-current-group :switch-key "kill other buffers")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(package-selected-packages
   '(projectile evil-commentary yasnippet-snippets yasnippet evil-surround lsp-ivy lsp-mode company envrc magit evil-collection general evil helpful counsel ivy-rich which-key rainbow-delimiters doom-themes nerd-icons doom-modeline ivy))
 '(safe-local-variable-values '((checkdoc-allow-quoting-nil-and-t . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
