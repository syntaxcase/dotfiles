;;; .emacs --- acc .emacs file
;;; Commentary:
;;  acc .emacs file

;;; Code:
;;; GC every 20MB allocated (instead of the default 0.76MB)
(setq gc-cons-threshold 6400000)
(setq read-process-output-max (* 1024 1024)) ;; 1MB

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)

;;; Packages
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

(setq frame-resize-pixelwise t)

(setq inhibit-compacting-font-caches t)

;; Make straight fly at boot: https://github.com/raxod502/straight.el#my-init-time-got-slower
(customize-set-variable 'straight-check-for-modifications '(watch-files find-when-checking))

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
;; (setq use-package-verbose t)
;; (setq use-package-compute-statistics t)

;;; font selection
;; (if (eq system-type 'darwin)
;;     (set-face-attribute 'default nil :font "Monaco-16")
;;                                         ; (set-face-attribute 'default nil :font "Ubuntu Mono" :height 138)
;;   (set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
;;   ;(set-face-attribute 'default nil :font "Iosevka" :height 143)
;;   )

;; Fix the emoji font problem when using emacsclient
;; thanks to: https://blog.mudit.xyz/posts/angels-and-daemons-a-tale-of-emojis-in-emacs
(defun acc/set-emoji-font ()
  (set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'prepend))

;; Call the config function once and then remove the handler
(defun acc/set-emoji-font-in-frame (frame)
  (with-selected-frame frame
    (acc/set-emoji-font))

  ;; Unregister this hook once it's run
  (remove-hook 'after-make-frame-functions
               #'acc/set-emoji-font-in-frame))

;; Attach the function to the hook only if in Emacs server
;; otherwise just call the config function directly
(if (daemonp)
    (add-hook 'after-make-frame-functions
              #'acc/set-emoji-font-in-frame)
  (acc/set-emoji-font))


;;;; emacs confirm closing
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; period single space ends sentence
(setq sentence-end-double-space nil)

;;;; disable suspend
(global-unset-key (kbd "C-z"))

;; I always want to kill the current buffer!
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;;; Window configuration
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell t)

;;; expand kill-ring size
(setq kill-ring-max 500)

;;; Enable narrowing
(put 'narrow-to-region 'disabled nil)

;; Stretch cursor to cover long characters (like TABs)
(setq x-stretch-cursor t)

;;; paste from mouse where the point is, not where the mouse pointer is
(setq mouse-yank-at-point t)

;;; Revert a buffer if the corresponding file changes on disk
(global-auto-revert-mode 1)

;;; Enable tab-bar-mode by default
;(setq tab-bar-show 1)
;; (setq tab-bar-new-tab-to 'rightmost)
;; (setq tab-bar-new-tab-choice "*scratch*")
;; (setq tab-bar-close-button-show nil)
;; (setq tab-bar-tab-hints t)
;; (tab-bar-mode 1)

;;;; set tab width
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;;;; disable line-wrapping
(setq-default truncate-lines t)

;;;; default to 100 columns
(setq-default fill-column 100)

;;;; Don't use native GTK tooltips
(setq x-gtk-use-system-tooltips nil)

;;;; Set title format
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;;; Backup and auto-save files
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

;;; always enable subword-mode when programming
(add-hook 'prog-mode-hook #'subword-mode)

;;; handle long lines better
(global-so-long-mode 1)

;;; flyspell-prog-mode
;(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(setq js-indent-level 2)

(straight-use-package 'use-package)

(use-package doom-themes
  :straight t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package unicode-fonts
   :ensure t
   :config
   (unicode-fonts-setup))

(use-package uniquify
  ; don't :ensure, provided by emacs
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package bufler
  :straight t
  :bind (("C-x C-b" . bufler)
         ("C-x b" . bufler-workspace-switch-buffer)))

;; (use-package persistent-scratch
;;   :straight t
;;   :config
;;   (persistent-scratch-setup-default))

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package minions
  :straight t
  :custom
  (minions-mode-line-lighter "#")
  :config
  (minions-mode 1))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-env-enable-python nil)
  (doom-modeline-env-enable-go nil)
  (doom-modeline-env-enable-rust nil)
  (doom-modeline-env-enable-elixir nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-icon t))

(use-package which-key
  :straight t
  :config (which-key-mode))

(use-package free-keys
  :straight t
  :commands free-keys)

(use-package whole-line-or-region
  :straight t
  :config
  (whole-line-or-region-global-mode))

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

;; hydra
(use-package hydra
  :straight t
  :config
  (defhydra hydra-flymake (global-map "C-c f")
    "flymake"
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("q" nil)))


;; (use-package tree-sitter
;;   :straight (tree-sitter :type git
;;                          :host github
;;                          :repo "ubolonton/emacs-tree-sitter"
;;                          :files ("lisp/*.el"))
;;   :config
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;                '(rustic-mode . rust)))

;; (use-package tree-sitter-langs
;;   :straight (tree-sitter-langs :type git
;;                                :host github
;;                                :repo "ubolonton/emacs-tree-sitter"
;;                                :files ("langs/*.el" "langs/queries"))
;;   :after tree-sitter)

(use-package selectrum
  :straight t
  :config
  (selectrum-mode))

(use-package prescient
  :straight t
  :config (prescient-persist-mode +1))

(use-package selectrum-prescient
  :straight t
  :after (prescient selectrum)
  :config
  (selectrum-prescient-mode +1))

(use-package marginalia
  :straight (marginalia :type git
                        :host github
                        :repo "minad/marginalia"
                        :branch "main"
                        :files ("*.el"))
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package consult
  :straight (consult :type git
                     :host github
                     :repo "minad/consult"
                     :files ("*.el"))
  ;; Replace bindings. Lazily loaded due to use-package.
  :bind (("C-c h" . consult-history)
         ("C-c o" . consult-outline)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r x" . consult-register)
         ("C-x r b" . consult-bookmark)
         ("M-g o" . consult-outline) ;; "M-s o" is a good alternative
         ("M-g m" . consult-mark)    ;; "M-s m" is a good alternative
         ("M-g l" . consult-line)    ;; "M-s l" is a good alternative
         ("M-s m" . consult-multi-occur)
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Replace functions (consult-multi-occur is a drop-in replacement)
  (fset 'multi-occur #'consult-multi-occur)

  ;; Configure other variables and modes in the :config section, after lazily loading the package
  :config

  ;; Optionally enable previews. Note that individual previews can be disabled
  ;; via customization variables.
  (consult-preview-mode))

(use-package ctrlf
  :straight t
  :config (ctrlf-mode +1))

(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char)
         ("C-c '" . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :config (avy-setup-default))

(use-package volatile-highlights
  :straight t
  :hook (after-init . volatile-highlights-mode)
  :config
  (volatile-highlights-mode t))

(use-package fic-mode
  :straight t
  :commands fic-mode
  :hook (prog-mode))

(use-package iedit
  :bind ("C-;". iedit-mode)
  :straight t)

(use-package dumb-jump
  :straight t
  :bind (("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)))

(use-package goto-chg
  :straight t
  :commands goto-last-change
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package smartparens
  :straight t
  :demand t
  :config
  (use-package smartparens-config)
  (use-package smartparens-python)
  (smartparens-global-mode t)
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package company
  :straight t
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (global-company-mode))

(use-package company-quickhelp
  :after company
  :straight t
  :bind (:map company-active-map
              ("C-c h" . #'company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1))

(use-package detour
  :straight t
  :bind (("s-." . detour-mark)
         ("s-," . detour-back)))

(use-package undo-tree
  :straight t
  :custom
  (undo-tree-enable-undo-in-region t)
  :config
  (global-undo-tree-mode))

(use-package hl-anything
  :straight t
  :commands hl-highlight-mode
  :bind
  (("<f7> <f7>" . 'hl-highlight-thingatpt-local)
   ("<f7> u" . 'hl-unhighlight-all-local)
   ("<f7> U" . 'hl-unhighlight-all-global)
   ("<f7> n" . 'hl-find-next-thing)
   ("<f7> p" . 'hl-find-prev-thing)))

(use-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(use-package rainbow-delimiters
  :straight t
  :commands rainbow-delimiters-mode)

(use-package projectile
  :straight t
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "node_modules"))
  :config
  (projectile-mode))

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (magit-revision-show-gravatars t))

(use-package magit-todos
  :straight t
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :straight t
  :after magit
  :custom
  (auth-sources '("~/.authinfo.gpg"))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :straight t
    :commands (all-the-icons-dired-mode)))

(use-package org
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
  :config
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

;; (use-package ox-reveal
;;  :straight t
;;  :after (org))

(use-package org-superstar
 :straight t
 :after (org)
 :config (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(use-package org-roam
  :straight t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package org-roam-server
  :straight t
  :after org-roam
  :commands org-roam-server-mode)

;; (use-package lister
;;   :straight (lister :type git
;;                     :host github
;;                     :repo "publicimageltd/lister"
;;                     :branch "main"
;;                     :files ("*.el")))

;; (use-package delve
;;   :straight (delve :type git
;;                     :host github
;;                     :repo "publicimageltd/delve"
;;                     :branch "main"
;;                     :files ("*.el"))
;;   :config
;;   (use-package delve-minor-mode
;;     :config
;;     (add-hook 'org-mode-hook #'delve-minor-mode-maybe-activate))
;;   :bind
;;   (("<f12>" . delve-open-or-select)))

(use-package org-journal
  :straight t
  :bind (("C-c C-j" . org-journal-new-entry))
  :after org
  :custom
  (org-journal-dir "~/Dropbox/org/journal/"))

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

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
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :straight t
  :commands (cider cider-connect cider-jack-in)
  :custom
  (cider-default-cljs-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))"))

(use-package paredit
  :straight t
  :commands enable-paredit-mode
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          clojure-mode)
         . enable-paredit-mode))

(use-package haskell-mode
  :straight t
  :mode "\\.hs\\'"
  :config (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package flymake-shellcheck
  :straight t
  :commands flymake-shellcheck-load
  :config
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package python
  :straight t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  (python-shell-interpreter "python3"))

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

(use-package flycheck
  :straight t
  ;; :init
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :bind (("s-<f6>" . next-error)
         ("s-<f5>" . previous-error))
  :config
  (global-flycheck-mode))

(use-package flycheck-rust
  :straight t
  :after flycheck)

(use-package treemacs
  :straight t
  :commands treemacs
  :bind (("M-0" . treemacs-select-window)
         ("<f8>" . treemacs)))

(use-package treemacs-projectile
  :straight t
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :straight t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :straight t
  :after treemacs magit)

(use-package lsp-treemacs
  :straight t
  :after treemacs lsp-mode
  :config
  (lsp-treemacs-sync-mode 1))

(use-package csv-mode
  :straight t
  :mode "\\.csv\\'")

(use-package go-mode
  :straight t
  :mode  "\\.go\\'"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package lsp-mode
  :straight t
  :after company
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (python-mode . lsp-deferred)
	 (elixir-mode . lsp-deferred)
         (dockerfile-mode . lsp-deferred)
         (lsp-managed-mode . lsp-diagnostics-modeline-mode))
  :custom
  (lsp-rust-clippy-preference "on")
  (lsp-rust-server 'rust-analyzer)
  (lsp-prefer-capf t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-modeline-diagnostics-enable t)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-delay 1)
  (lsp-ui-sideline-update-mode 'point)
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  (mapc (lambda (f) (set-face-foreground f "dim gray"))
        '(lsp-ui-sideline-code-action
          lsp-ui-sideline-current-symbol
          lsp-ui-sideline-symbol
          lsp-ui-sideline-symbol-info)))

(use-package java-mode
  :mode "\\.java\\'")

(use-package lsp-java
  :straight t
  :after (lsp-mode java-mode))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

; (use-package dap-java :after (lsp-java dap-mode))
(use-package dap-go
  :after (lsp-mode dap-mode)
  :config (dap-go-setup))

(use-package dap-python
  :after lsp-mode
  :custom
  (dap-python-executable "python3"))


(use-package deadgrep
  :straight t
  :bind ("<f5>" . deadgrep))

;;;; Rust stuff
;; mostly taken from: http://bassam.co/emacs/2015/08/24/rust-with-emacs/
;; Setting up configurations when you load rust-mode
(use-package rust-mode
  :straight t
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t))

(use-package auctex
  :straight t
  :mode ("\\.tex\\'" . latex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)

  :config
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package reftex
  :straight t
  :after (auctex)
  :init (setq reftex-plug-into-AUCTeX t))

(use-package json-mode
  :straight t
  :mode "\\.json\\'")

;;; Javascript/Typescript
(use-package prettier-js
  :straight t
  :commands prettier-js-mode
  :custom
  (prettier-js-args '("--trailing-comma" "all" "--single-quote" "--semi" "--arrow-parens" "always")))

(use-package typescript-mode
  :straight t
  :mode "\\.tsx?\\'")

(use-package tide
  :straight t
  :commands tide-setup
  :custom
  (typescript-indent-level 2)
  (tide-format-options '(:indentSize 2 :tabSize 2)))

;; HACK: I can't figure out a way to make tide depend on the built-in js-mode
;; using only the `use-package' machinery. So to avoid having tide take half
;; a second at startup it can be delayed on the `tide-setup' command, with
;; the hook on js-mode (to invoke `tide-setup') set outside of `use-package'.
(add-hook 'js-mode-hook (lambda () (tide-setup)))
(add-hook 'js-mode-hook #'prettier-js-mode)
(add-hook 'typescript-mode-hook #'(lambda () (tide-setup)))
;(add-hook 'typescript-mode-hook #'prettier-js-mode)

;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-mode))

;;; AsciiDoc mode
(use-package adoc-mode
  :straight t
  :mode "\\.adoc\\'")

;;; YAML mode
(use-package yaml-mode
  :straight t
  :mode "\\.yaml\\'")

;;; toml mode
(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

;;; plantuml
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  (plantuml-output-type "txt"))

;;; Groovy
(use-package groovy-mode
  :ensure    t
  :mode      "\\.\\(groovy\\|gradle\\)$")

;; Set up the basic Elixir mode.
(use-package elixir-mode
  :straight t
  :commands elixir-mode
  :mode "\\.exs\\?\\'"
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package erlang
  :straight t
  :mode "\\.erl\\?\\'")

(use-package php-mode
  :straight t
  :mode "\\.php\\'")

(use-package simple-httpd
  :straight t
  :commands httpd-start)

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
              ("g" . mu4e-update-mail-and-index))
  :config
  ;; thanks https://groups.google.com/forum/#!topic/mu-discuss/JqHEGycEyKI
  (defun my-mu4e-action-view-with-xwidget (msg)
    "View the body of the message inside xwidget-webkit."
    (unless (fboundp 'xwidget-webkit-browse-url)
      (mu4e-error "No xwidget support available"))
    (let* ((html (mu4e-message-field msg :body-html))
           (txt (mu4e-message-field msg :body-txt))
           (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
      (unless (or html txt)
        (mu4e-error "No body part for this message"))
      (with-temp-buffer
        ;; simplistic -- but note that it's only an example...
        (insert (or html (concat "<pre>" txt "</pre>")))
        (write-file tmpfile)
        (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

  (add-to-list 'mu4e-view-actions
               '("webkit" . my-mu4e-action-view-with-xwidget) t))

(use-package verb
  :straight t
  :mode ("\\.verb\\'" . verb-mode))

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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'acc/smarter-move-beginning-of-line)

(global-set-key (kbd "C-c m s") 'acc/swap-quotes)
(global-set-key (kbd "C-c m a") 'acc/beginning-of-string)
(global-set-key (kbd "C-c m +") 'acc/increment-number-at-point)
(global-set-key (kbd "C-c m {") 'acc/space-js-object)

;; Reset GC threshold, it was set in ~/.emacs.d/early-init.el
; (setq gc-cons-threshold +old-gc-cons-threshold+)

(provide '.emacs)

(provide 'init)
;;; init ends here
