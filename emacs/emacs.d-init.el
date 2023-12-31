;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;  acc's init.el file

;;; Code:
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)

;;; Packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(setq frame-resize-pixelwise t)

(setq inhibit-compacting-font-caches t)

;; Make straight fly at boot: https://github.com/raxod502/straight.el#my-init-time-got-slower
(customize-set-variable 'straight-check-for-modifications '(watch-files find-when-checking))
(customize-set-variable 'straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;;; font selection
;; (if (eq system-type 'darwin)
;;     (set-face-attribute 'default nil :font "Monaco-16")
;;                                         ; (set-face-attribute 'default nil :font "Ubuntu Mono" :height 138)
;;   (set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
;;   ;(set-face-attribute 'default nil :font "Iosevka" :height 143)
;;   )

;; Fix the emoji font problem when using emacsclient
;; thanks to: https://blog.mudit.xyz/posts/angels-and-daemons-a-tale-of-emojis-in-emacs
;; (defun acc/set-emoji-font ()
;;   ;; (set-fontset-font "fontset-default" 'emoji
;;   ;;                   '("Noto Color Emoji" . "iso10646-1") nil 'prepend)
;;   (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'prepend))

;; ;; Call the config function once and then remove the handler
;; (defun acc/set-emoji-font-in-frame (frame)
;;   (with-selected-frame frame
;;     (acc/set-emoji-font))

;;   ;; Unregister this hook once it's run
;;   (remove-hook 'after-make-frame-functions
;;                #'acc/set-emoji-font-in-frame))

;; ;; Attach the function to the hook only if in Emacs server
;; ;; otherwise just call the config function directly
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               #'acc/set-emoji-font-in-frame)
;;   (acc/set-emoji-font))

(use-package emacs
  :init
  ;;; Silence the warnings from native-comp
  (setq native-comp-async-report-warnings-errors nil)

  ;; emacs confirm closing
  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; period single space ends sentence
  (setq sentence-end-double-space nil)

  ;; the minibuffer stays in the frame that created it
  (setq minibuffer-follows-selected-frame nil)

  ;; disable suspend
  (global-unset-key (kbd "C-z"))

  ;; I always want to kill the current buffer!
  (global-set-key (kbd "C-x k") 'kill-this-buffer)

  ;; Bind view-mode to F12
  (global-set-key (kbd "<f12>") 'view-mode)

  ;; Window configuration
  (setq column-number-mode t)
  (setq show-paren-context-when-offscreen 'child-frame)
  (show-paren-mode t)

  ;; Allow isearch motion with M-<, M->, C-v and M-v. New in emacs 28
  (setq isearch-allow-motion t)
  (setq isearch-lazy-count t)

  ;; Kill the previous dired buffer when selecting a new directory. New in emacs 28
  (setq dired-kill-when-opening-new-dired-buffer t)

  ;; expand kill-ring size
  (setq kill-ring-max 500)

  ;; Enable narrowing
  (put 'narrow-to-region 'disabled nil)

  ;; Stretch cursor to cover long characters (like TABs)
  (setq x-stretch-cursor t)

  ;; paste from mouse where the point is, not where the mouse pointer is
  (setq mouse-yank-at-point t)

  ;; Revert a buffer if the corresponding file changes on disk
  (global-auto-revert-mode 1)

  ;; Enable recentf-mode
  (recentf-mode 1)

  ;; Enable pixel-scroll-precision-mode
  (pixel-scroll-precision-mode 1)

  ;;; Enable tab-bar-mode by default
  ;; (use-package tab-bar
  ;;   :custom
  ;;   (tab-bar-show t)
  ;;   (tab-bar-new-tab-to 'rightmost)
  ;;   (tab-bar-new-tab-choice "*scratch*")
  ;;   (tab-bar-close-button-show nil)
  ;;   (tab-bar-tab-hints t)
  ;;   :config
  ;;   (tab-bar-mode 1))

  ;; set tab width
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq-default indent-tabs-mode nil)
  (setq js-indent-level 2)

  ;; disable line-wrapping
  (setq-default truncate-lines t)

  ;; default to 100 columns
  (setq-default fill-column 100)

  ;; Don't use native GTK tooltips
  (setq use-system-tooltips nil)

  ;; Set title format
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; Backup and auto-save files
  (defvar my-backup-files-dir "~/.emacs.d/backup-files")
  (setq backup-directory-alist `(("." . ,my-backup-files-dir)))
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  (setq auto-save-file-name-transforms
        `((".*" ,my-backup-files-dir t)))

  ;; Save whatever’s in the current (system) clipboard before
  ;; replacing it with the Emacs’ text.
  ;; https://github.com/dakrone/eos/blob/master/eos.org
  (setq save-interprogram-paste-before-kill t)

  ;; always enable subword-mode when programming
  (add-hook 'prog-mode-hook #'subword-mode)

  ;; flyspell
  ;; (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)


(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode))

(use-package treesit
  :straight (:type built-in)
  :custom
  ;; Set the path for tree-sitter's language definitions
  (treesit-extra-load-path '("/home/acc/.emacs.d/tree-sitter/dist"))
  :config
  ;; Replace relevant modes with the treesitter variant
  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (css-mode        . css-ts-mode)
             ;; (dockerfile-mode . dockerfile-ts-mode)
             ;; (go-mode         . go-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (clojure-mode    . clojure-ts-mode)
             ;; (typescript-mode . typescript-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mode)))
(use-package modus-themes
  :straight (modus-themes :host github :repo "protesilaos/modus-themes")
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package eldoc
  :straight (:type built-in)
  :config
  (global-eldoc-mode))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package mood-line
  :straight t
  :config
  (mood-line-mode))

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :config (which-key-mode))

(use-package helpful
  :straight t
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ace-window
(use-package ace-window
  :straight t
  :custom
  (aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package vertico
  :straight (vertico :host github :repo "minad/vertico")
  :config
  (vertico-mode))

(use-package savehist
  :straight (:type built-in)
  :init
  (savehist-mode))

(use-package orderless
  :straight t
  :after vertico
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))
  
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-completion
  :straight t
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (("C-c h" . consult-history)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ("M-g !" . consult-flymake)
         ("M-g i" . consult-imenu)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ([remap repeat-complex-command] . consult-complex-command))
  :init
  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package consult-lsp
  :straight t
  :after lsp-mode
  :bind ([remap xref-find-apropos] . #'consult-lsp-symbols))

(use-package embark
  :straight t
  :bind
  ("C-S-a" . embark-act))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package ctrlf
  :straight t
  :bind
  (("C-s" . ctrlf-forward-default)
   ("C-r" . ctrlf-backward-default))
  :config
  (ctrlf-mode +1))

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char)
         ("C-c '" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)))

(use-package jinx
  :straight t
  :hook ((text-mode . jinx-mode)
         (conf-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct))

(use-package apheleia
  :straight t
  :hook ((go-ts-mode . apheleia-mode)
         (elixir-ts-mode . apheleia-mode)
         (rust-ts-mode . apheleia-mode)))

(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

(use-package iedit
  :straight t
  :bind ("C-;". iedit-mode))

(use-package goto-chg
  :straight t
  :commands goto-last-change
  :bind (("s-/" . goto-last-change)
         ("s-," . goto-last-change-reverse)))

(use-package smartparens
  :straight t
  :config
  (use-package smartparens-config)
  :hook (((go-ts-mode python-ts-mode js-ts-mode rust-ts-mode sql-mode) . smartparens-mode)
         ((clojure-mode lisp-interaction-mode emacs-lisp-mode elixir-ts-mode) . smartparens-strict-mode))
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

;; (use-package company
;;   :straight t
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-minimum-prefix-length 2)
;;   (company-tooltip-align-annotations t)
;;   (company-dabbrev-downcase nil)
;;   (company-show-numbers t)
;;   (company-format-margin-function #'company-vscode-dark-icons-margin)
;;   :config
;;   (global-company-mode))

;; (use-package company-quickhelp
;;   :after company
;;   :straight t
;;   :bind (:map company-active-map
;;               ("C-c h" . #'company-quickhelp-manual-begin))
;;   :config
;;   (company-quickhelp-mode 1))

(use-package corfu
  :straight (corfu :type git :host github :repo "minad/corfu")
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto-prefix 2)
  :config
  (global-corfu-mode))

;; (use-package undo-tree
;;   :straight t
;;   :custom
;;   (undo-tree-enable-undo-in-region t)
;;   (undo-tree-auto-save-history nil)
;;   :config
;;   (global-undo-tree-mode))

(use-package vundo
  :straight (vundo :type git :host github :repo "casouri/vundo")
  :commands (vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-revision-show-gravatars t)
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package magit-todos
  :straight t
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package magit-delta
  :straight t
  :after magit
  :hook (magit-mode . magit-delta-mode))

(use-package forge
  :straight t
  :after magit
  :custom
  (auth-sources '("~/.authinfo.gpg")))

(use-package org
  :straight (:type built-in)
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto))
  :custom
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate nil)
  (org-startup-indented t)
  (org-html-doctype "html5")

  (org-log-done 'time)

  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))
  (org-catch-invisible-edits 'error)
  :init
  ;; Not in :custom because `org-element-use-cache' is a `defvar'
  (setq org-element-use-cache nil)

  :config
  ;; (add-hook 'org-mode-hook 'flyspell-mode)
  (require 'ox-md)
  (add-to-list 'org-export-backends 'markdown)

  ;; (require 'ox-deck)

  ;; (add-to-list 'org-src-lang-modes '("js" . js2))
  ;; (add-to-list 'org-src-lang-modes '("deck-js" . js2))

  ;; (defvar org-babel-default-header-args:deck-js
  ;;   '((:results . "html")
  ;;     (:exports . "results")))

  ;; (defun org-babel-execute:deck-js (body params)
  ;;   (let ((ext-lib (assoc :data-external-libs params)))
  ;;     (if ext-lib
  ;;         (format "<code class=\"javascript\" data-external-libs=\"%s\">\n%s\n</code>" (cdr ext-lib) body)
  ;;         (format "<code class=\"javascript\">\n%s\n</code>" body))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (awk . t)
     (sed . t)
     (shell . t)
     (js . t)
     (python . t))))

(use-package toc-org
  :straight t
  :after (org)
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-journal
  :straight t
  :bind (("C-c C-j" . org-journal-new-entry))
  :custom
  (org-journal-dir "~/Dropbox/org/journal/"))

(use-package olivetti
  :straight t
  :commands olivetti-mode)

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :mode "Dockerfile\\'")

(use-package docker
  :straight t
  :commands docker)

(use-package rego-mode
  :straight t
  :mode "\\.rego\\'")

(use-package clojure-mode
  :straight t
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.cljc$" . clojurec-mode))
  :config
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :straight t
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-eldoc-display-for-symbol-at-point nil)
  (cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

;; (use-package paredit
;;   :straight t
;;   :commands enable-paredit-mode
;;   :hook ((emacs-lisp-mode
;;           lisp-mode
;;           ; lisp-interaction-mode
;;           scheme-mode
;;           clojure-mode)
;;          . enable-paredit-mode))

;; (use-package slime
;;   :straight t
;;   :commands slime
;;   :init
;;   (setq slime-lisp-implementations '((sbcl ("sbcl"))))
;;   (setq slime-default-lisp 'sbcl)
;;   :config
;;   (slime-setup '(slime-fancy slime-cl-indent)))

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package python
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

(use-package jinja2-mode
  :straight (jinja2-mode :type git :host github :repo "paradoxxxzero/jinja2-mode")
  :mode "\\.j2\\'")

(use-package scala-mode
  :straight t
  :mode "\\.scala\\'")

(use-package yasnippet
  :straight t
  :after (:any lsp-mode tide)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (use-package yasnippet-snippets
    :straight t)
  (yas-global-mode 1))

;; Using this only to treat any diretory that contains a .project file as a project.
(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90))

(use-package treemacs
  :straight t
  :commands treemacs
  :bind (("M-0" . treemacs-select-window)
         ("<f8>" . treemacs)))

(use-package treemacs-icons-dired
  :straight t
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :straight t
  :after ( treemacs magit))

(use-package lsp-treemacs
  :straight t
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")

(use-package go-ts-mode
  :straight (:type built-in)
  :mode  "\\.go\\'")

(use-package go-mod-ts-mode
  :straight (:type built-in)
  :mode  "go\\.mod\\'")

(use-package lsp-mode
  :straight t
  :bind-keymap
  ("C-c d" . lsp-command-map)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode)
         (go-ts-mode . lsp-deferred)
         (go-mod-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
	 (elixir-ts-mode . lsp-deferred)
         (dockerfile-ts-mode . lsp-deferred)
         (lsp-managed-mode . lsp-diagnostics-modeline-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-rust-clippy-preference "on")
  (lsp-rust-server 'rust-analyzer)
  (lsp-prefer-capf t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-go-build-flags ["-tags=integration,debug"])
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 1)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-doc-position 'top)
  ;;(lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-show-with-cursor t)
  ;;(lsp-ui-doc-use-webkit t)
  (lsp-ui-peek-list-width 100)
  :config
  (mapc (lambda (f) (set-face-foreground f "dim gray"))
        '(lsp-ui-sideline-code-action
          lsp-ui-sideline-current-symbol
          lsp-ui-sideline-symbol
          lsp-ui-sideline-symbol-info)))

;; (use-package toolbox-tramp
;;   :straight (toolbox-tramp :type git
;; 			     :host github
;; 			     :repo "fejfighter/toolbox-tramp")
;;   :custom
;;   (toolbox-tramp-flatpak-wrap t)) ; Use `flatpak-spawn' when conecting

;; (use-package docker-tramp
;;   ; :commands (docker-tramp-add-method)
;;   :custom
;;   (docker-tramp-docker-executable "podman")
;;   :straight t)

(use-package java-ts-mode
  :straight (:type built-in)
  :mode "\\.java\\'")

(use-package lsp-java
  :straight t
  :after (lsp-mode))

;; (use-package dap-mode
;;   :straight t
;;   :after lsp-mode
;;   :config
;;   (dap-mode t)
;;   (dap-ui-mode t))

; (use-package dap-java :after (lsp-java dap-mode))

;; (use-package dap-python
;;   :after lsp-mode
;;   :custom
;;   (dap-python-executable "python3"))

;; (use-package pyvenv
;;   :straight t
;;   ; :hook (python-mode . pyvenv-mode)
;;   :config
;;   ; thanks https://www.fusionbox.com/blog/detail/exploring-large-and-unfamiliar-python-projects-in-emacs/640/
;;   (defun acc/configure-python-venv ()
;;     "Set `python-shell-virtualenv-root' to the virtualenv directory."
;;     (interactive)
;;     (require 'projectile)
;;     (let* ((virtualenv-path
;;             (file-truename
;;              (concat (locate-dominating-file default-directory "Pipfile") ".venv/"))))
;;       (when (file-directory-p virtualenv-path)
;;         (message (format "Activating virtualenv %s" virtualenv-path))
;;         (pyvenv-activate virtualenv-path))))

;;   (add-hook 'python-mode-hook #'acc/configure-python-venv))

(use-package deadgrep
  :straight t
  :bind ("<f5>" . deadgrep))

(use-package rust-ts-mode
  :straight (:type built-in)
  :mode "\\.rs\\'")

;; (use-package auctex
;;   :straight t
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :init
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (setq TeX-PDF-mode t)

;;   :config
;;   (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
;;   (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
;;   (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;   ; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

;; (use-package reftex
;;   :straight t
;;   :after (auctex)
;;   :init (setq reftex-plug-into-AUCTeX t))

(use-package json-ts-mode
  :straight t
  :mode "\\.json\\'")

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode "\\.tsx\\'")

(use-package js-ts-mode
  :straight (:type built-in)
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)))

;;; AsciiDoc mode
(use-package adoc-mode
  :straight t
  :mode "\\.adoc\\'")

;;; YAML
(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package yaml-pro
  :straight t
  :after yaml-ts-mode
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

;;; toml mode
(use-package toml-ts-mode
  :straight (:type built-in)
  :mode "\\.toml\\'")

;; Terraform
(use-package terraform-mode
  :straight t
  :mode "\\.tf\\'")

;;; plantuml
(use-package plantuml-mode
  :straight t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "txt"))

;;; Groovy
(use-package groovy-mode
  :straight t
  :mode      "\\.\\(groovy\\|gradle\\)$")

;;; Erlang and Elixir
(use-package elixir-ts-mode
  :straight t
  :mode (("\\.exs\\?\\'" . elixir-ts-mode)
         ("mix\\.lock" . elixir-ts-mode)))

(use-package erlang
  :straight t
  :mode "\\.erl\\?\\'")

(use-package php-mode
  :straight t
  :mode "\\.php\\'")

(use-package bart-mode
  :straight t
  :custom
  (bart-station 'nbrk)
  (bart-manage-window t)
  :commands bart)

(use-package smtpmail
  :straight t
  :after mu4e
  :custom
  (user-mail-address "alessandro.carlo@chiri.co")
  (smtpmail-default-smtp-server "mail.gandi.net")
  (smtpmail-smtp-server "mail.gandi.net")
  (smtpmail-stream-type 'ssl)
  (smtpmail-smtp-service 465)
  (user-full-name "Alessandro Carlo Chirico")
  (auth-sources '("~/.authinfo.gpg")))

(use-package smtpmail-async
  :after smtpmail
  :custom
  (send-mail-function 'async-smtpmail-send-it)
  (message-send-mail-function 'async-smtpmail-send-it))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :custom
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-hide-index-messages t)
  (mu4e-update-interval nil)
  (mu4e-change-filenames-when-moving t)
  (mu4e-completing-read-function 'completing-read)
  (mu4e-compose-signature-auto-include nil)
  (mu4e-maildir "~/Maildir")
  (mu4e-sent-folder   "/chirico-gandi/Sent")
  (mu4e-drafts-folder "/chirico-gandi/Drafts")
  (mu4e-trash-folder  "/chirico-gandi/Trash")
  (mu4e-refile-folder "/chirico-gandi/Archive")
  (mu4e-sent-messages-behavior 'sent)
  (message-kill-buffer-on-exit nil)
  (mu4e-view-show-addresses t)
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'ask)
  (message-citation-line-format "On %Y-%m-%d, %R (%Z), %f wrote:\n")
  (message-citation-line-function 'message-insert-formatted-citation-line)
  :bind (:map mu4e-main-mode-map
              ("g" . mu4e-update-mail-and-index)))

(use-package mu4e-views
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views" :branch "mu-1.8-support")
  :after mu4e
  :bind (:map mu4e-headers-mode-map
              ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
              ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
              ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
              ("f" . mu4e-views-toggle-auto-view-selected-message))) ;; toggle opening messages automatically when moving in the headers view

(use-package speed-type
  :straight t
  :commands speed-type-text)

;;;; My utility functions!
(defun acc/point-in-string-p (pt)
  "Return t if PT is in a string."
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun acc/beginning-of-string ()
  "Move to the beginning of a syntactic string."
  (interactive)
  (unless (acc/point-in-string-p (point))
    (error "You must be in a string for this command to work"))
  (while (acc/point-in-string-p (point))
    (forward-char -1))
  (point))

;; Thanks to https://www.masteringemacs.org/article/swapping-quote-symbols-emacs-parsepartialsexp
(defun acc/swap-quotes ()
  "Swap the quote symbols in a string."
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (acc/beginning-of-string)))
          (eos (save-excursion
                 (acc/beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
        (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

(defun acc/beginning-of-js-object ()
  "Find the beginning of a js object {}."
  (while (not (char-equal ?\{ (following-char)))
    (forward-char -1))
  (point))

(defun acc/space-js-object ()
  "Insert one space after the opening `{' and before the closing `}'."
  (interactive)
  (save-excursion
    (let ((boo (save-excursion
                 (acc/beginning-of-js-object))))
      (goto-char boo)
      (forward-char)
      (just-one-space)
      (goto-char boo)
      (forward-sexp)
      (forward-char -1)
      (just-one-space))))

;; From https://www.emacswiki.org/emacs/IncrementNumber
(defun acc/increment-number-at-point ()
  "Increments the number under point."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; Thanks Alessio and thanks http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun acc/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Thanks https://emacs.stackexchange.com/questions/12613/convert-the-first-character-to-uppercase-capital-letter-using-yasnippet
(defun acc/capitalize-first-char (&optional string)
  "Capitalize only the first character of the input STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

;; Thanks to https://emacs.stackexchange.com/questions/38156/reversing-the-order-of-letters-characters-of-a-selected-region
(defun acc/reverse-region (beg end)
  "Reverse characters between BEG and END."
  (interactive "r")
  (let ((region (buffer-substring beg end)))
    (delete-region beg end)
    (insert (nreverse region))))

;; `base64-decode-region' supports not inserting line breaks, but not when called interactively (as far as I can tell),
;; so here's a variation that never inserts line breaks.
;; Thanks to https://emacs.stackexchange.com/a/41224
(defun acc/base64-encode-region-no-breaklines ()
  (interactive)
  (base64-encode-region (mark) (point) t))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'acc/smarter-move-beginning-of-line)

(global-set-key (kbd "C-c m s") 'acc/swap-quotes)
(global-set-key (kbd "C-c m a") 'acc/beginning-of-string)
(global-set-key (kbd "C-c m +") 'acc/increment-number-at-point)
(global-set-key (kbd "C-c m {") 'acc/space-js-object)
(global-set-key (kbd "C-c m r") 'acc/reverse-region)

;; thanks: https://old.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/h4f0dsq/
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 6400000)
            (setq read-process-output-max (* 1024 1024)))) ;; 1MB

;; thanks: https://old.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)

(provide '.emacs)

(provide 'init)
;;; init.el ends here
