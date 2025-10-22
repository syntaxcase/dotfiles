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

;;;; Elpaca installer, copied from its docs.
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;;;; End elpaca installer.

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(setq use-package-verbose t)
(setq use-package-compute-statistics t)

(elpaca elpaca-use-package
  ;;Enable Elpaca's use-package support
  (elpaca-use-package-mode))
;; Block until package is installed/activated so we can use it at the top-level below.
(elpaca-wait)

(defvar my-backup-files-dir "~/.emacs.d/backup-files")

(use-package emacs
  :ensure nil
  :custom
  ;; Silence the warnings from native-comp
  (native-comp-async-report-warnings-errors nil)
  (confirm-kill-emacs 'yes-or-no-p)
  ;; period single space ends sentence
  (sentence-end-double-space nil)

  ;; Isearch
  ;; Allow isearch motion with M-<, M->, C-v and M-v. New in emacs 28
  (isearch-allow-motion t)
  ;; show the number of matches
  (isearch-lazy-count t)
  ;; but do prefix the number
  (lazy-count-prefix-format nil)
  ;; append it
  (lazy-count-suffix-format "   (%s/%s)")

  (show-paren-context-when-offscreen 'child-frame)

  ;; expand kill-ring size
  (kill-ring-max 500)

  ;; rename via git
  (dired-vc-rename-file t)

  ;; Kill the previous dired buffer when selecting a new directory. New in emacs 28
  (dired-kill-when-opening-new-dired-buffer t)

  ;; paste from mouse where the point is, not where the mouse pointer is
  (mouse-yank-at-point t)

  ;; Backup and auto-save files
  (backup-directory-alist `(("." . ,my-backup-files-dir)))
  (auto-save-file-name-transforms `((".*" ,my-backup-files-dir t)))
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 2)
  (version-control t)

  ;; project.el: additional project directory markers
  (project-vc-extra-root-markers '( ".project.el" ".projectile" ))

  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; the minibuffer stays in the frame that created it
  (setq minibuffer-follows-selected-frame nil)

  ;; Enable narrowing
  (put 'narrow-to-region 'disabled nil)

  ;; Stretch cursor to cover long characters (like TABs)
  (setq x-stretch-cursor t)

  ;; set tab width
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq-default indent-tabs-mode nil)
  (setq js-indent-level 2)

  ;; disable line-wrapping
  (setq-default truncate-lines t)

  ;; default to 100 columns
  (setq-default fill-column 100)

  ;; Echo keystrokes in the modeline as fast as possible
  (setq echo-keystrokes .1)

  ;; Don't use native GTK tooltips
  (setq use-system-tooltips nil)

  ;; Set title format
  (setq frame-title-format
        (list (format "%s %%S: %%j " (system-name))
              '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

  ;; Save whatever’s in the current (system) clipboard before
  ;; replacing it with the Emacs’ text.
  ;; https://github.com/dakrone/eos/blob/master/eos.org
  (setq save-interprogram-paste-before-kill t)

  ;; flyspell
  ;; (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  :config
  ;; always enable subword-mode when programming
  (add-hook 'prog-mode-hook #'subword-mode)

  (global-set-key (kbd "C-x j") #'duplicate-dwim)

  ;; disable suspend
  (global-unset-key (kbd "C-z"))

  ;; I always want to kill the current buffer!
  (global-set-key (kbd "C-x k") 'kill-current-buffer)

  ;; Bind view-mode to F12
  ;; (global-set-key (kbd "<f12>") 'view-mode)

  ;; Revert a buffer if the corresponding file changes on disk
  (global-auto-revert-mode 1)

  ;; Enable recentf-mode
  (recentf-mode 1)

  ;; Enable pixel-scroll-precision-mode
  (pixel-scroll-precision-mode 1)

  (show-paren-mode t))

(use-package emacs
  :ensure nil
  :init
  (setq server-client-instructions nil)
  :config
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(use-package emacs
  :ensure nil
  :config
  (require-theme 'modus-themes)
  (load-theme 'modus-vivendi-tinted :no-confirm))

;;; Enable tab-bar-mode by default
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show t)
  (tab-bar-auto-width nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-tab-hints nil)
  :config
  (tab-bar-mode 1))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

(use-package treesit
  :ensure nil
  :custom
  ;; Set the path for tree-sitter's language definitions
  (treesit-font-lock-level 4)
  (java-ts-mode-indent-offset 2)
  :config
  ;; Replace relevant modes with the treesitter variant
  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (css-mode        . css-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (clojure-mode    . clojure-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode))
  (add-to-list 'treesit-language-source-alist '(erlang "https://github.com/WhatsApp/tree-sitter-erlang")))

(use-package transient
  :ensure t)

(use-package uniquify
  :ensure nil
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-flag t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode))

(use-package cond-let
  :ensure (cond-let :host github :repo "tarsius/cond-let"))

;; Expand-region replacement based on tree-sitter
(use-package expreg
  :ensure (expreg :host github :repo "casouri/expreg")
  :bind ("C-=" . expreg-expand))

;; (use-package expand-region
;;   :ensure t
;;   :bind ("C-=" . er/expand-region))

(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ace-window
(use-package ace-window
  :ensure t
  :custom
  (aw-scope 'frame)
  :bind ("M-o" . ace-window))

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(use-package vertico
  :ensure (vertico :host github :repo "minad/vertico")
  :config
  (vertico-mode)
  (vertico-multiform-mode)
  (vertico-indexed-mode)
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4))))

(use-package prescient
  :ensure t)

(use-package vertico-prescient
  :ensure t
  :after (prescient vertico))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package orderless
  :ensure t
  :after vertico
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles )))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package consult
  :ensure t
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
  :ensure t
  :after lsp-mode
  :bind ([remap xref-find-apropos] . #'consult-lsp-symbols))

(use-package embark
  :ensure t
  :bind
  ("C-S-a" . embark-act)
  :init
  (setq embark-indicators
        '(embark-minimal-indicator  ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  ;; Setting this in an `:init' block to make it work right from boot
  ;; or it'll work only after embark is loaded via the keybinding above.
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package consult-flycheck
  :ensure t
  :after flycheck
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))

(use-package flyover
  :ensure (:host github :repo "konrad1977/flyover" :branch "main" :files ("flyover.el"))
  :hook (flycheck-mode . flyover-mode))

(use-package avy
  :ensure t
  :custom (avy-all-windows nil)
  :bind (("M-g '" . avy-goto-char)
         ("M-g \"" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g t" . avy-goto-char-timer)))

(use-package jinx
  :ensure t
  :hook ((text-mode . jinx-mode)
         (conf-mode . jinx-mode))
  :bind ([remap ispell-word] . jinx-correct))

(use-package apheleia
  :ensure t
  :hook ((go-ts-mode . apheleia-mode)
         ;;; (elixir-ts-mode . apheleia-mode)
         (rust-ts-mode . apheleia-mode)))

(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse nil))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package goto-chg
  :ensure t
  :commands goto-last-change
  :bind (("s-/" . goto-last-change)
         ("s-," . goto-last-change-reverse)))

(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  :hook (((go-ts-mode python-ts-mode js-ts-mode rust-ts-mode sql-mode elixir-ts-mode) . smartparens-mode)
         ((clojure-ts-mode lisp-interaction-mode emacs-lisp-mode) . smartparens-strict-mode))
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

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  (company-format-margin-function #'company-vscode-dark-icons-margin)
  :hook (prog-mode . company-mode))

(use-package company-quickhelp
  :after company
  :ensure t
  :bind (:map company-active-map
              ("C-c h" . #'company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1))

(use-package company-prescient
  :ensure t
  :after (prescient company))

;; (use-package corfu
;;   :ensure t
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   (corfu-auto-prefix 2)
;;   :hook (prog-mode . corfu-mode))

;; (use-package undo-tree
;;   :straight t
;;   :custom
;;   (undo-tree-enable-undo-in-region t)
;;   (undo-tree-auto-save-history nil)
;;   :config
;;   (global-undo-tree-mode))

(use-package vundo
  :ensure (vundo :host github :repo "casouri/vundo")
  :commands (vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

(use-package wgrep
  :ensure t
  :after consult)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-revision-show-gravatars t)
  ;; Enable nerd icons support; added in https://github.com/magit/magit/commit/223461b52c35b0f426c053f4c6e7e7637c4a9b73.
  (magit-format-file-function #'magit-format-file-nerd-icons)
  :config
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))

(use-package difftastic
  :ensure t
  :after magit
  :bind (:map magit-blame-read-only-mode-map
              ("D" . difftastic-magit-diff)
              ("S" . difftastic-magit-show))
  :config
  (transient-append-suffix 'magit-diff '(-1 -1)
    [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
     ("S" "Difftastic show" difftastic-magit-show)]))

(use-package forge
  :ensure t
  :after magit
  :custom
  (auth-sources '("~/.authinfo.gpg")))

(use-package org
  :ensure nil
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

  (org-hide-emphasis-markers t)

  (org-html-doctype "html5")

  (org-log-done 'time)

  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))

  ;; forbid editing in folded areas, it's just too confusing.
  (org-fold-catch-invisible-edits 'error)

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
  :ensure t
  :after (org)
  :config
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-appear
  :ensure (org-appear :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  (org-roam-database-connector 'sqlite-builtin)
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
  :ensure (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-journal
  :ensure t
  :bind (("C-c C-j" . org-journal-new-entry))
  :custom
  (org-journal-dir "~/Dropbox/org/journal/"))

(use-package dslide
  :ensure (dslide :host github :repo "positron-solutions/dslide")
  :commands dslide-deck-start)

(use-package moc
  :ensure (moc :host github :repo "positron-solutions/moc")
  :after dslide)

(use-package olivetti
  :ensure t
  :commands olivetti-mode
  :bind (("<f6>" . olivetti-mode)))

(use-package otpp
  :ensure t
  :after project
  :init
  ;; If you like to define some aliases for better user experience
  (defalias 'one-tab-per-project-mode 'otpp-mode)
  (defalias 'one-tab-per-project-override-mode 'otpp-override-mode)
  :config
  (otpp-mode 1)
  ;; If you want to advice the commands in `otpp-override-commands`
  ;; to be run in the current's tab (so, current project's) root directory
  (otpp-override-mode 1))

;;; Erlang and Elixir
(use-package elixir-ts-mode
  :ensure nil
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("mix\\.lock" . elixir-ts-mode))
  :hook (before-save . lsp-format-buffer))

(use-package erlang
  :ensure t
  :mode "\\.erl\\?\\'")

(use-package dockerfile-ts-mode
  :ensure nil
  :mode "Dockerfile\\'")

(use-package docker
  :ensure t
  :commands docker)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package rego-mode
  :ensure t
  :mode "\\.rego\\'")

(use-package clojure-ts-mode
  :ensure t
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.cljc$" . clojurec-mode))
  :config
  (add-hook 'clojure-ts-mode-hook #'subword-mode))

(use-package cider
  :ensure t
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

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

(use-package jinja2-mode
  :ensure (jinja2-mode :host github :repo "paradoxxxzero/jinja2-mode")
  :mode "\\.j2\\'")

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'")

(use-package yasnippet
  :ensure t
  :after (:any lsp-mode tide)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode 1))

(use-package treemacs
  :ensure t
  :commands treemacs
  :bind (("M-0" . treemacs-select-window)
         ("<f8>" . treemacs)))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package project-treemacs
  :ensure t
  :after treemacs
  :config
  (project-treemacs-mode))

(use-package treemacs-nerd-icons
  :ensure t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package go-ts-mode
  :ensure nil
  :mode  "\\.go\\'")

(use-package go-mod-ts-mode
  :ensure nil
  :mode  "go\\.mod\\'")

(use-package lua-ts-mode
  :ensure nil
  :mode "\\.lua\\'")

(use-package lsp-mode
  :ensure t
  :bind-keymap
  ("C-c d" . lsp-command-map)
  :hook ((lsp-mode . lsp-lens-mode)
         (go-ts-mode . lsp-deferred)
         (go-mod-ts-mode . lsp-deferred)
         (clojure-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (elixir-ts-mode . lsp-deferred)
         (dockerfile-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (lsp-managed-mode . lsp-diagnostics-modeline-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion))
  :custom
  (lsp-elixir-ls-version "v0.15.0")
    ;;; (lsp-completion-provider :none) ;; we use Corfu!
  (lsp-rust-clippy-preference "on")
  (lsp-rust-server 'rust-analyzer)
  (lsp-prefer-capf t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-go-build-flags ["-tags=integration,debug,integrationnew"])
  (lsp-typescript-format-enable nil)
  (lsp-javascript-format-enable nil)
  (typescript-indent-level 2)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
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
;; 			   :host github
;; 			   :repo "fejfighter/toolbox-tramp"))

;; (use-package docker-tramp
;;   ; :commands (docker-tramp-add-method)
;;   :custom
;;   (docker-tramp-docker-executable "podman")
;;   :straight t)

(use-package java-ts-mode
  :ensure nil
  :mode "\\.java\\'")

(use-package lsp-java
  :ensure t
  :custom
  (lsp-java-format-enabled nil)
  ; (lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.1/jdt-language-server-1.46.1-202504011455.tar.gz")
  (lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms100m" "-javaagent:/home/acc/.emacs.d/lombok.jar"))
  :after lsp-mode)

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
  :ensure t
  :bind ("<f5>" . deadgrep))

(use-package rust-ts-mode
  :ensure nil
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

(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'")

(use-package tsx-ts-mode
  :ensure nil
  :mode "\\.tsx\\'")

;; This can probably be removed because of `major-mode-remap-alist'
;; (use-package js-ts-mode
;;   :straight (:type built-in)
;;   :mode (("\\.js\\'" . js-ts-mode)
;;          ("\\.jsx\\'" . js-ts-mode)))

;;; YAML
(use-package yaml-ts-mode
  :ensure nil
  :mode ("\\.ya?ml\\'" . yaml-ts-mode))

(use-package yaml-pro
  :ensure t
  :after yaml-ts-mode
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

;; Prism
;; (use-package prism
;;   :ensure (prism :host github :repo "alphapapa/prism.el")
;;   :commands (prism-mode prism-whitespace-mode)
;;   ;; :hook ((yaml-ts-mode . prism-whitespace-mode)
;;   ;;        (json-ts-mode . prism-mode))
;;   :config
;;   ;; Taken from https://protesilaos.com/emacs/modus-themes#h:a94272e0-99da-4149-9e80-11a7e67a2cf2
;;   (setq prism-num-faces 16)

;;   (prism-set-colors
;;    :desaturations '(0) ; do not change---may lower the contrast ratio
;;    :lightens '(0)      ; same
;;    :colors (modus-themes-with-colors
;;              (list fg-main
;;                    magenta
;;                    cyan-cooler
;;                    magenta-cooler
;;                    blue
;;                    magenta-warmer
;;                    cyan-warmer
;;                    red-cooler
;;                    green
;;                    fg-main
;;                    cyan
;;                    yellow
;;                    blue-warmer
;;                    red-warmer
;;                    green-cooler
;;                    yellow-faint))))

;;; toml mode
(use-package toml-ts-mode
  :ensure nil
  :mode "\\.toml\\'")

;; Terraform
(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'")

;; cue mode
(use-package cue-mode
  :ensure t
  :mode "\\.cue\\'")

;;; plantuml
;; (use-package plantuml-mode
;;   :ensure t
;;   :mode "\\.plantuml\\'"
;;   :custom
;;   (plantuml-jar-path "/usr/share/java/plantuml.jar")
;;   (plantuml-default-exec-mode 'jar)
;;   (plantuml-output-type "txt"))

;;; Groovy
(use-package groovy-mode
  :ensure t
  :mode      "\\.\\(groovy\\|gradle\\)$")

(use-package php-ts-mode
  :ensure nil
  :mode  "\\.php\\'")

(use-package nginx-mode
  :ensure t
  :mode "nginx\\.conf")

(use-package eat
  :ensure (eat :host codeberg
               :repo "akib/emacs-eat"
               :files ("*.el" ("term" "term/*.el") "*.texi"
                       "*.ti" ("terminfo/e" "terminfo/e/*")
                       ("terminfo/65" "terminfo/65/*")
                       ("integration" "integration/*")
                       (:exclude ".dir-locals.el" "*-tests.el")))
  :custom (eat-term-scrollback-size 1048576)
  :commands eat)

;; typst
(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode")
  :mode "\\.typ\\'"
  :custom
  (typst-ts-mode-watch-options "--open"))

(use-package bart-mode
  :ensure t
  :custom
  (bart-station 'nbrk)
  (bart-manage-window t)
  :commands bart)

(use-package speed-type
  :ensure t
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
