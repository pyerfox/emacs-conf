;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;;("org" . "https://orgmode.org/elpa/")
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
(setq auto-save-default nil)

(setq select-enable-clipboard nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 10)

(setq gc-cons-threshold (* 1024 1024 100))
(setq read-process-output-max (* 1024 1024))

(use-package dash :defer t)
(use-package s :defer t)
(use-package f :defer t)
(use-package pythonic :defer t)

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
(set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))
;; (setq face-font-rescale-alist '(("D2Coding" . 1.3)))

;;(use-package doom-themes
;;  :config
;;  (load-theme 'doom-monokai-machine t))
(load-theme 'tsdh-light t)

;; need to run (all-the-icons-install-fonts) once
;; (use-package doom-modeline  
;;   :config
;;   (doom-modeline-mode 1)
;;   (setq doom-modeline-height 1)
;;   (set-face-attribute 'mode-line nil :height 80)
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :height 80 :background "#202020"))
;; :custom-face
;; (mode-line ((t (:height 1))))
;; (mode-line-inactive ((t (:height 1 :background "#202020")))) 
;; :config
;; (doom-modeline-height 1))

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

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config
;;   (setq projectile-track-known-projects-automatically nil)
;;   (projectile-mode)
;;   :demand t
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map))
;;   ;; :init
;;   ;; (when (file-directory-p "~/Projects/Code")
;;   ;;   (setq projectile-project-search-path '("~/Projects/Code")))

;; (use-package counsel-projectile
;;   :after projectile
;;   ;; :bind (("C-M-p" . counsel-projectile-find-file))
;;   :config
;;   (counsel-projectile-mode))

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
  (setq which-key-idle-delay 1.0))

(defun ecfg/dired-mode-setup ()
  (visual-line-mode -1)
  (setq truncate-lines t)
  (evil-collection-define-key 'normal 'dired-mode-map
    ;;"H" 'dired-up-directory
    ;;"L" 'dired-find-file))
    "H" 'dired-single-up-directory
    "L" 'dired-single-buffer))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind
  ("C-x C-j" . dired-jump)
  ;;:custom
  ;;(dired-listing-switches "-agho --group-directories-first")
  :hook (dired-mode . ecfg/dired-mode-setup))

(use-package dired-single)

;;(use-package dired-open
;;  :config
;;  (setq dired-open-extensions nil))

(use-package dired-hide-dotfiles)

(use-package autorevert
  :config (global-auto-revert-mode 1))

(use-package paredit
  :disabled t)
  ;; :diminish paredit-mode
  ;; :hook ((emacs-lisp-mode lisp-mode racket-mode) . paredit-mode))

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
;;   :prefix "SPC")

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

;; (ecfg/leader-key-def
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
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  ;; (setq evil-want-fine-undo t)
  ;; (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "i") 'evil-insert-state)
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
  ;; (set-face-attribute 'org-document-title nil :weight 'bold :height 1.4)
  ;; (dolist (face '((org-level-1 . 1.4)
  ;;                 (org-level-2 . 1.2)
  ;;                 (org-level-3 . 1.2)
  ;;                 (org-level-4 . 1.0)
  ;;                 (org-level-5 . 1.0)
  ;;                 (org-level-6 . 1.0)
  ;;                 (org-level-7 . 1.0)
  ;;                 (org-level-8 . 1.0)))
  ;;   (set-face-attribute (car face) nil :weight 'medium :height (cdr face)))
  )

(use-package org
  :defer t
  :hook (org-mode . ecfg/org-mode-setup)
  :config
  (setq org-directory "C:/MyData/Workspace/org/")
  ;;(setq org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "|" "DONE(d)" "CANCELED(c)" )))
  ;;(setq org-log-done 'time)
  ;;(setq org-log-done 'note)
  (setq org-agenda-files '("C:/MyData/Workspace/org/gtd/next.org"
                           "C:/MyData/Workspace/org/gtd/project.org"))
  (setq org-default-notes-file '("C:/MyData/Workspace/org/capture.org"))
  (setq org-M-RET-may-split-line nil)
  (setq gtd/next-action-head "Next actions:")
  (setq gtd/waiting-head "Waiting on:")
  (setq gtd/complete-head "Completed items:")
  (setq gtd/project-head "Projects:")
  (setq gtd/someday-head "Someday/maybe:")
  (setq org-agenda-custom-commands
        '(
          ("g" "GTD view"
           ((agenda)
            (todo "NEXT" ((org-agenda-overriding-header gtd/next-action-head)))
            (todo "WAITING" ((org-agenda-overriding-header gtd/waiting-head)))
            (todo "DONE" ((org-agenda-overriding-header gtd/complete-head)))
            (tags "PROJECTS-SOMEDAY" ((org-agenda-overriding-header gtd/project-head)))
            (tags "SOMEDAY"  ((org-agenda-overriding-header gtd/someday-head)))
            ))))
)
        ;; org-ellipsis " ▼"
        ;; org-hide-emphasis-markers t
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
  (org-superstar-headline-bullets-list '("◎" "○" "●" "○" "●" "○" "●")))


(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("tt" . "src text"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("pu" . "src plantuml"))

;;(custom-set-faces
;; ;; '(org-block-begin-line
;; ;;   ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
;; '(org-block
;;   ((t (:background "#202030"))))
;; ;; '(org-block-end-line
;; ;;   ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
;; )

(custom-set-faces
 '(org-block
   ((t (:background "#EEEEEE"))))
 )

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (plantuml . t)
   ))

(setq org-plantuml-exec-mode 'plantuml)
(setq org-confirm-babel-evaluate nil)

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

(use-package lsp-mode
  :custom
  (lsp-enable-snippet nil)
  (lsp-idle-delay 1.0)
  :config
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c C-l") lsp-command-map))

(use-package lsp-ui
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))
  ;; :config
  ;; ;;(setq lsp-ui-peek-enable t)
  ;; ;;(setq lsp-ui-peek-always-show t)
  ;; (setq lsp-ui-sideline-show-diagnostics t)
  ;; (setq lsp-ui-sideline-show-code-actions t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq-default
   flycheck-python-pycompile-executable "python"
   flycheck-go-gofmt-executable "goimports"
   flycheck-go-vet-executable "go"
   flycheck-go-staticcheck-executable "staticcheck"
   flycheck-standard-error-navigation nil
   flycheck-disabled-checkers '(racket python-mypy go-staticcheck)
   flycheck-emacs-lisp-load-path 'inherit))

(use-package company
  :diminish company-mode
  :hook (prog-mode . company-mode)
  ;; :bind (:map company-active-map
  ;;             ("<tab>" . company-complete-selection))
  ;;       (:map lsp-mode-map
  ;;             ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 1.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(use-package eval-in-repl)

(defun ecfg/eir-slime-setup ()
  ;(setq eir-jump-after-eval nil)
  (local-set-key (kbd "<C-return>") 'eir-eval-in-slime))

(require 'eval-in-repl-slime)
(add-hook 'lisp-mode-hook #'ecfg/eir-slime-setup)

(use-package go-mode
  :config
  (setq gofmt-command "goimports"))
  ;; (add-hook 'before-save-hook #'gofmt-before-save))

(defun ecfg/go-mode-setup ()
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (setq tab-width 4)
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  (lsp-deferred))
(add-hook 'go-mode-hook #'ecfg/go-mode-setup)

(use-package python-mode
  ;;:after conda
  :custom
  (py-shell-name "python"))
  ;;(python-shell-interpreter "ipython")
  ;;(python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"))

(defun ecfg/python-mode-setup ()
  (require 'eval-in-repl-python)
  (local-set-key (kbd "<C-return>") 'eir-eval-in-python)
  (require 'lsp-pyright)
  (lsp-deferred))

(use-package lsp-pyright
  :custom (lsp-pyright-typechecking-mode "off")
  :hook (python-mode . ecfg/python-mode-setup))

;;(use-package conda
;;  :load-path "vendor/conda"
;;  :config
;;  ;; if you want interactive shell support, include:
;;  (conda-env-initialize-interactive-shells)
;;  ;; if you want eshell support, include:
;;  (conda-env-initialize-eshell)
;;  (conda-env-activate "base") 
;;  :custom
;;  (conda-anaconda-home "C:/ProgramData/Miniconda3"))

(use-package racket-mode
  :mode "\\.rkt\\'"
  :after flycheck
  :preface
  ;; (defun bp-insert-lisp-section (section)
  ;;   "Insert a LISP section header with SECTION at point."
  ;;   (interactive "sSection: ")
  ;;   (let ((suffix (s-repeat (- 72 (length section) 4) ";")))
  ;;     (insert (format ";; %s %s\n" section suffix))))
  (defvar bp-racket-defun-likes
    '(call-with-browser!
      call-with-browser-script!
      call-with-database-connection
      call-with-database-transaction
      call-with-element-screenshot!
      call-with-encrypted-output
      call-with-hmac-output
      call-with-input-bytes
      call-with-input-string
      call-with-marionette!
      call-with-page
      call-with-page!
      call-with-page-pdf!
      call-with-page-screenshot!
      call-with-pdf-from-uri
      call-with-persistent-database-connection
      call-with-pk
      call-with-pool-connection
      call-with-pool-resource
      call-with-postmark-connection
      call-with-pubsub-events
      call-with-redis
      call-with-redis-client
      call-with-redis-pool
      call-with-redis-pubsub
      call-with-screenshot
      call-with-semaphore
      call-with-test-client+server
      call-with-transaction
      call-with-twilio-connection
      call-with-unzip
      call-with-waiter
      case/dep
      case/enum
      case-view
      for/stream
      form*
      gen:let
      let*
      let-globals
      place
      property
      section
      serializable-struct
      serializable-struct/versions
      struct++
      system-test-suite
      test
      test-commands
      tpl:xexpr-when
      xexpr-unless
      xexpr-when))
  (defun bp-racket-mode-hook ()
    (interactive)
    (setq adaptive-fill-mode t))
  :config
  (add-hook 'racket-mode-hook #'bp-racket-mode-hook)

  (flycheck-define-checker racket-review
    "check racket source code using racket-review"
    :command ("raco" "review" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
    :modes racket-mode)
  (add-to-list 'flycheck-checkers 'racket-review)
  (setq racket-repl-buffer-name-function
        #'racket-repl-buffer-name-project
        racket-show-functions
        '(racket-show-echo-area))

  (dolist (id bp-racket-defun-likes)
    (put id 'racket-indent-function #'defun)))

  ;; (require 'eval-in-repl-racket)
  ;; (define-key racket-mode-map (kbd "<C-return>") 'eir-eval-in-racket)

 ;; :bind (:map racket-mode-map
 ;;             ("{"       . paredit-open-curly)
 ;;             ("}"       . paredit-close-curly)
 ;;             ("C-c C-d" . racket-xp-describe)
 ;;             ("C-c C-r" . racket-xp-rename)
 ;;             ;; ("C-c C-s" . bp-insert-lisp-section)
 ;;             ("C-c r t" . racket-tidy-requires)
 ;;             ("C-c r i" . racket-add-require-for-identifier)
 ;;             ("C-c ."   . racket-xp-visit-definition)
 ;;             ("C-c ,"   . racket-unvisit)))

(require 'racket-xp)
(add-hook 'racket-mode-hook #'racket-xp-mode)

(use-package scribble-mode
  :mode "\\.scrbl\\'")

(use-package pollen-mode
  :disabled t
  :mode "\\.p[mp]?\\'")

(use-package json-snatcher :defer t)
(use-package json-reformat :defer t)

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2
        js-indent-level 2))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; (use-package web-mode
;;   :mode (("\\.html?\\'"        . web-mode))
;;          ;; ("\\.mjml\\'"         . web-mode)
;;          ;; ("\\.vue\\'"          . web-mode)
;;          ;; ("\\.hbs\\'"          . web-mode)
;;          ;; ("\\.eex\\'"          . web-mode)
;;          ;; ("\\.tm?pl\\'"        . web-mode)
;;          ;; ("\\.blade\\.php\\'"  . web-mode))
;;   :config
;;   (setq web-mode-code-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-style-indent-offset 2
;;         web-mode-script-indent-offset 2
;;         web-mode-markup-indent-offset 2
;; 
;;         web-mode-style-padding 2
;;         web-mode-script-padding 2
;; 
;;         web-mode-enable-auto-closing t
;;         web-mode-enable-auto-expanding t
;;         web-mode-enable-auto-pairing t
;;         web-mode-enable-current-element-highlight t))
;;         ;; web-mode-content-types-alist '(("jsx" . "\\.mjs\\'"))
;;         ;; web-mode-engines-alist '(("django" . "\\.html\\'"))))

(require 'tcl)
(defun ecfg/tcl-eval-line-or-region (start end &optional and-go)
  (interactive "r\nP")
  (if (region-active-p)
    (call-interactively #'tcl-eval-region)
    (save-mark-and-excursion
      (beginning-of-line)
      (set-mark-command nil)
      (end-of-line)
      (tcl-eval-region (region-beginning) (region-end) and-go)
      (pop-mark))))

(defun ecfg/tcl-setup ()
  (local-set-key (kbd "<C-return>") #'ecfg/tcl-eval-line-or-region))

(add-hook 'tcl-mode-hook #'ecfg/tcl-setup)
