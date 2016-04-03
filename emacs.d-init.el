;;; .emacs --- acc .emacs file
;;; Commentary:
;;  acc .emacs file

;;; Code:
;;; GC every 20MB allocated (instead of the default 0.76MB)
;;; This is commented out while I test the GC trick below
; (setq gc-cons-threshold 20000000)

;;; Stop GCs while in the minibuffer.
;;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'req-package)
  (package-install 'req-package))
(require 'req-package)

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 138)

;;;; emacs confirm closing
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; disable suspend
(global-unset-key (kbd "C-z"))

;; Extend PATH environment variable. Emacs doesn't source .zshrc
;; so it doesn't see executables in ~/bin
(setenv "PATH" (concat "/home/acc/sw/scala-2.11.7/bin:"
                       "/home/acc/sw/npm/bin/:"
                       "/home/acc/sw/node-v5.7.1-linux-x64/bin/:"
                       "/home/acc/bin:"
                       (getenv "PATH")))

(dolist (dir '("/home/acc/bin"
               "/home/acc/sw/npm/bin/"
               "/home/acc/sw/node-v5.7.1-linux-x64/bin/"
               "/home/acc/sw/scala-2.11.7/bin"))
  (add-to-list 'exec-path dir))

;;;; GUIX env vars
;; (setenv "LOCPATH" "$HOME/.guix-profile/lib/locale")
;; (setenv "GUILE_LOAD_PATH" "/home/acc/.guix-profile/share/guile/site/2.0")
;; (setenv "GUILE_LOAD_COMPILED_PATH" "/home/acc/.guix-profile/share/guile/site/2.0")
;; (setenv "CPATH" "/home/acc/.guix-profile/include")
;; (setenv "LIBRARY_PATH" "/home/acc/.guix-profile/lib")

;;;; Window configuration
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell t)
(setq create-lockfiles nil)

;;;; set tab width
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;;;; disable line-wrapping
(setq-default truncate-lines t)

;;;; Bind C-x o to M-o
(global-set-key (kbd "M-o") 'other-window)

;;;; Set title format
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;;; Backup files
(setq backup-directory-alist `(("." . "~/.emacs.d/backup-files")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(req-package solarized-theme
  :init
  (progn
    (setq x-underline-at-descent-line t)
    (load-theme 'solarized-dark t)))

(req-package winner
  :defer t
  :config (winner-mode 1))

(req-package anzu
  :defer t
  :diminish anzu-mode
  :config (global-anzu-mode +1))

(req-package uniquify
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(req-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(req-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(req-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (global-unset-key (kbd "C-x c"))
    (setq helm-M-x-fuzzy-match                  t
          helm-bookmark-show-location           t
          helm-buffers-fuzzy-matching           t
          helm-completion-in-region-fuzzy-match t
          helm-file-cache-fuzzy-match           t
          helm-imenu-fuzzy-match                t
          helm-mode-fuzzy-match                 t
          helm-locate-fuzzy-match               t
          helm-quick-update                     t
          helm-recentf-fuzzy-match              t
          helm-semantic-fuzzy-match             t))
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files))
  :config (helm-mode 1))

(req-package avy
  :bind (("C-c :" . avy-goto-char)
         ("C-c '" . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :config (avy-setup-default))

(req-package iedit)

(req-package goto-chg
  :commands goto-last-change
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(req-package spaceline
  :init (setq powerline-default-separator 'wave)
  :config
  (progn
    (require 'spaceline-config)
    (spaceline-emacs-theme)
    (spaceline-helm-mode)))

(req-package company
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  :config (global-company-mode))

(req-package company-tern
  :require company
  :init
  (setq company-tern-property-marker "")
  :config
  (add-to-list 'company-backends 'company-tern))

(req-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(req-package projectile
  :require helm
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "node_modules"))
  :config
  (projectile-global-mode)
  (helm-projectile-on))

(req-package org-bullets
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(req-package clojure-mode
  :defer t
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(req-package paredit
  :defer t
  :init
  (let ((h (lambda ()
             (paredit-mode +1)
             (turn-on-eldoc-mode))))
    (add-hook 'emacs-lisp-mode-hook h)
    (add-hook 'lisp-mode-hook h)
    (add-hook 'lisp-interaction-mode-hook h)
    (add-hook 'scheme-mode-hook h)
    (add-hook 'clojure-mode-hook h)))

(req-package slime
  :defer t
  :init
  (setq inferior-lisp-program "/home/acc/bin/sbcl")
  ;; (setq inferior-lisp-program "/home/acc/sw/ccl-1.9/lx86cl -K utf-8")
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-compiler-notes-tree))

  ;; CLHS
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t))

(req-package haskell-mode
  :defer t
  :init (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(req-package python-mode
  :defer t
  :init (setq python-shell-interpreter "python3"))

;;;; Rust stuff
;; mostly taken from: http://bassam.co/emacs/2015/08/24/rust-with-emacs/
;; Set path to racer binary
(req-package racer
  :init
  (setq racer-cmd "/home/acc/bin/racer")
  ;; Set path to rust src directory
  (setq racer-rust-src-path "/home/acc/src/upstream/rust/src/"))

(req-package company-racer
  :require (racer company)
  :config
  (add-to-list 'company-backend 'company-racer))

;; Setting up configurations when you load rust-mode
(req-package rust-mode
  :require (company-racer flycheck)
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            '(lambda ()
               ;; Enable racer
               (racer-activate)

               ;; Hook in racer with eldoc to provide documentation
               (racer-turn-on-eldoc)

               ;; Use flycheck-rust in rust-mode
               (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

               ;; Use company-racer in rust mode
               (set (make-local-variable 'company-backends) '(company-racer))

               ;; Key binding to jump to method definition
               (local-set-key (kbd "M-.") #'racer-find-definition)

               ;; Key binding to auto complete and indent
               (local-set-key (kbd "TAB") #'company-indent-or-complete-common))))

(req-package flyspell
  :defer t
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list")
  :config
  ;; I don't want flyspell-auto-correct-word bound to C-M-i, C-. is enough.
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(req-package auctex
  :defer t
  :require flyspell
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

(req-package json-mode
  :defer t
  :mode "\\.json\\'")

(req-package reftex
  :require auctex
  :defer t
  :init (setq reftex-plug-into-AUCTeX t))

(req-package tern)

(req-package js2-mode
  :require (flycheck tern)
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t))))

(req-package web-mode
  :require (flycheck tern)
  :defer t
  :mode ("\\.jsx$" . web-mode)
  :init
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  :config
  (add-hook 'web-mode-hook (lambda () (tern-mode t))))

(req-package flycheck
  :config
  (progn
    (global-flycheck-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)))

(req-package-finish)

;;;;;;;
;; Automatically generated
;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "6a925fdf3a7bf2f3901d8fbc4ef64f9b4b4be2c6bed2b0d49d154db0bec91b33" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "9b94a52c25ea76b72df2050928d18e7fe9060e9c7f7d992f33bf35d4931b0444" "de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" default)))
 '(safe-local-variable-values
   (quote
    ((Package . DRAKMA)
     (TeX-master . t)
     (common-lisp-style . modern)
     (Base . 10)
     (Package . HUNCHENTOOT)
     (Syntax . COMMON-LISP)
     (Package . CXML)
     (Encoding . utf-8)
     (readtable . runes)
     (Package . SAX)
     (Syntax . Common-Lisp))))
 '(whitespace-style
   (quote
    (face tabs spaces trailing lines space-before-tab indentation empty space-after-tab space-mark tab-mark))))

(provide '.emacs)

;;; .emacs ends here
