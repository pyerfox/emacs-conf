
;;(set-langauge-environment "Korean")
(prefer-coding-system 'utf-8)
(set-file-name-coding-system 'cp949-dos)
(global-unset-key (kbd "S-SPC"))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(setq visible-bell t)

(setq make-backup-files nil)
(setq create-lockfiles nil)

(setq x-select-enable-clipboard nil)

(set-face-attribute 'default nil
                    :font "Fira Code"
                    :weight 'normal
                    :height 100)
;; (set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))
;; (setq face-font-rescale-alist '(("D2Coding" . 1.3)))

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
(require 'use-package)
(setq use-package-always-ensure t)

(use-package doom-themes
  :config
  (load-theme 'doom-monokai-machine t))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; run once (all-the-icons-install-fonts)
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
  

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (define-key help-map "\C-h" 'which-key-C-h-dispatch)
  (setq which-key-idle-delay 0.3))

;; (defun dw/evil-hook ()
;;   (dolist (mode '(custom-mode
;;                   eshell-mode
;;                   git-rebase-mode
;;                   erc-mode
;;                   circe-server-mode
;;                   circe-chat-mode
;;                   circe-query-mode
;;                   sauron-mode
;;                   term-mode))
;;   (add-to-list 'evil-emacs-state-modes mode)))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  ;;(add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; ;;(unless dw/is-termux
  ;;   ;; Disable arrow keys in normal and visual modes
  ;;   (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro);;)

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

(use-package hydra
  :defer t)

(use-package ivy-hydra
  :defer t
  :after hydra)

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(use-package general
  :config
  (general-evil-setup t)
  (general-define-key
   "<escape>" 'keyboard-esacpe-quit
   "C-M-j" 'counsel-switch-buffer)
  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :global-prefix "M-SPC"))
 
;; (defhydra hydra-text-scale (:timeout 4)
;;   "scale text"
;;   ("j" text-scale-increase "in")
;;   ("k" text-scale-decrease "out")
;;   ("f" nil "finished" :exit t))
;; (dw/leader-key-def "ts" '(hydra-text-scale/body :which-key "scale text"))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode)
(set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :weight 'medium :height (cdr face))))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)  
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



;;__________________________________________
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-indent org-superstar ivy-hydra hydra evil-collection evil undo-tree general doom-themes helpful counsel ivy-rich which-key rainbow-delimiters doom-modeline diminish swiper ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
