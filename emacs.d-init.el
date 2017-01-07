;;; .emacs --- acc .emacs file
;;; Commentary:
;;  acc .emacs file

;;; Code:
;;; GC every 20MB allocated (instead of the default 0.76MB)
;;; This is commented out while I test the GC trick below
; (setq gc-cons-threshold 20000000)

(defconst +rustc-src+ "/home/acc/src/upstream/rustc-1.14.0/src" "Path to rustc source.")

;;; Stop GCs while in the minibuffer.
;;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my-minibuffer-setup-hook ()
  "Disable garbage collection while in the minibuffer."
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  "Re-enable garbage collection when out of the minibuffer."
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)

;;; Packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))
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

;; I always want to kill the current buffer!
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Extend PATH environment variable. Emacs doesn't source .zshrc
;; so it doesn't see executables in ~/bin
(setenv "PATH" (concat "/home/acc/sw/scala-2.10.6/bin:"
                       "/home/acc/sw/npm/bin/:"
                       "/home/acc/sw/node-v5.7.1-linux-x64/bin/:"
                       "/home/acc/bin:"
                       "/home/acc/.cargo/bin:"
                       (getenv "PATH")))

(dolist (dir '("/home/acc/bin"
               "/home/acc/.cargo/bin/"
               "/home/acc/sw/npm/bin/"
               "/home/acc/sw/node-v5.7.1-linux-x64/bin/"
               "/home/acc/sw/scala-2.10.6/bin"))
  (add-to-list 'exec-path dir))

(setenv "RUST_SRC_PATH" +rustc-src+)

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

;;;; Prettify
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode 1)

(defun prettify-js-symbols-hook ()
  "Set pretty symbols for programming modes."
  (setq prettify-symbols-alist
        (append '(("===" . ?≡)
                  ("!==" . ?≢))
                prettify-symbols-alist)))

(add-hook 'web-mode-hook 'prettify-js-symbols-hook)
(add-hook 'js2-mode-hook 'prettify-js-symbols-hook)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;;;; Req packages
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
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

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
  :demand
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

(req-package volatile-highlights
  :config
  (volatile-highlights-mode t))

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
  :demand
  :require helm
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "node_modules"))
  :config
  (projectile-global-mode)
  (helm-projectile-on))

(req-package org
  (bind-key* "C-c c" 'org-capture)
  (bind-key* "C-c l" 'org-store-link)
  (bind-key* "C-c a" 'org-agenda)
  (bind-key* "C-c b" 'org-iswitch))

(req-package org-bullets
  :require org
  :defer t
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

(req-package ensime
  :pin melpa-stable
  :require helm
  :init (setq ensime-use-helm t))

(req-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

(req-package company-jedi
  :require (company jedi)
  :config
  (add-to-list 'company-backend 'company-jedi))

;;;; Rust stuff
;; mostly taken from: http://bassam.co/emacs/2015/08/24/rust-with-emacs/
;; Set path to racer binary
(req-package racer
  :init
  ;; Set path to rust src directory
  (setq racer-rust-src-path +rustc-src+))

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

(req-package helm-flycheck
  :require (helm flycheck)
  :defer t
  :bind (:map flycheck-mode-map
              ("C-c ! l" . helm-flycheck)))

(req-package flycheck
  :config
  (progn
    (global-flycheck-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)))

(req-package-finish)

;;; Org-mode customization found at:
;;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

(provide '.emacs)

(provide 'init)
;;; init ends here
