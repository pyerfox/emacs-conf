;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)         
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;(set-langauge-environment "Korean")
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'cp949-dos)

(global-unset-key (kbd "S-SPC"))

(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq x-select-enable-clipboard nil)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line number for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil
                    :font "Fira Code"
                    :weight 'normal
                    :height 100)
;; (set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))
;; (setq face-font-rescale-alist '(("D2Coding" . 1.3)))

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-machine t))

;; need to run (all-the-icons-install-fonts) once
(use-package doom-modeline  
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 80)
  (set-face-attribute 'mode-line-inactive nil
                      :height 80 :background "#202020"))
;  :custom-face
;  (mode-line ((t (:height 1))))
;  (mode-line-inactive ((t (:height 1 :background "#202020")))) 
;  :config
;  (doom-modeline-height 1))

(use-package magit
  ;; :bind ("C-M-;" . magit-status)
  ;; :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package diminish)
(use-package swiper)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package hydra
  :defer t)

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map))
  ;; :init
  ;; (when (file-directory-p "~/Projects/Code")
  ;;   (setq projectile-project-search-path '("~/Projects/Code")))

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
  (counsel-projectile-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (setq which-key-idle-delay 0.3))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package general
  :config
  (general-evil-setup)
  (general-define-key
   "<escape>" 'keyboard-esacpe-quit
   "C-M-j" 'counsel-switch-buffer))
  ;; (general-create-definer ecfg/leader-key-def
  ;;   :keymaps '(normal insert visual emacs)
  ;;   :prefix "M-SPC")

;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))
;; (ecfg/leader-key-def "ts" '(hydra-text-scale/body :which-key "scale text"))

;; (ecfg/leader-key-def
;;  "g"   '(:ignore t :which-key "git")
;;  "gs"  'magit-status
;;  "gd"  'magit-diff-unstaged
;;  "gc"  'magit-branch-or-checkout
;;  "gl"   '(:ignore t :which-key "log")
;;  "glc" 'magit-log-current
;;  "glf" 'magit-log-buffer-file
;;  "gb"  'magit-branch
;;  "gP"  'magit-push-current
;;  "gp"  'magit-pull-branch
;;  "gf"  'magit-fetch
;;  "gF"  'magit-fetch-all
;;  "gr"  'magit-rebase)

;; (dw/leader-key-def
;;   "pf"  'counsel-projectile-find-file
;;   "ps"  'counsel-projectile-switch-project
;;   "pF"  'counsel-projectile-rg
;;   ;; "pF"  'consult-ripgrep
;;   "pp"  'counsel-projectile
;;   "pc"  'projectile-compile-project
;;   "pd"  'projectile-dired)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(defun ecfg/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode)
  (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'medium :height (cdr face))))

(use-package org
  :defer t
  :hook (org-mode . ecfg/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
        org-agenda-files '()
        org-hide-emphasis-markers t
        ))
        ;; org-src-fontify-natively t
        ;; org-fontify-quote-and-verse-blocks t
        ;; org-src-tab-acts-natively t
        ;; org-edit-src-content-indentation 2
        ;; org-hide-block-startup nil
        ;; org-src-preserve-indentation nil
        ;; org-startup-folded 'content
        ;; org-cycle-separator-lines 2))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("■" "○" "●" "○" "●" "○" "●")))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#202030")))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

;; Automatically tangle this config file
(defun ecfg/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda () (add-hook
                      'after-save-hook
                      'ecfg/org-babel-tangle-config)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-superstar evil-collection evil general undo-tree rainbow-delimiters which-key helpful counsel-projectile projectile ivy-hydra hydra ivy-rich diminish counsel magit doom-modeline doom-themes use-package)))
