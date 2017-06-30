;;; .emacs --- acc .emacs file
;;; Commentary:
;;  acc .emacs file

;;; Code:
;;; GC every 20MB allocated (instead of the default 0.76MB)
(setq gc-cons-threshold (* 20 1024 1024))

(defconst +rustc-src+ "/home/acc/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")


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

(setenv "RUST_SRC_PATH" +rustc-src+)

;;;; Window configuration
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(setq column-number-mode t)
(show-paren-mode t)
(setq visible-bell t)
(setq create-lockfiles nil)

;;; paste from mouse where the point is, not where the mouse pointer is
(setq mouse-yank-at-point t)

;;; Revert a buffer if the corresponding file changes on disk
(global-auto-revert-mode 1)

;;;; set tab width
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;;;; disable line-wrapping
(setq-default truncate-lines t)

;;;; default to 100 columns
(setq-default fill-column 100)

;;;; Don't use native GTK tooltips
(setq x-gtk-use-system-tooltips nil)

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

(req-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(req-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(req-package better-shell
  :bind
  (("C-'" . better-shell-shell)
   ("C-=" . better-shell-remote-open)))

(req-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(req-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; ace-window
(req-package ace-window
  :bind ("M-p" . ace-window))

;; hydra
(req-package hydra
  :ensure t)

;; ivy
(req-package ivy
  :ensure t
  :diminish ivy-mode
  :require hydra
  :bind (("C-c C-r" . ivy-resume)
         ("C-s" . swiper)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . ivy-switch-buffer)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")
  (setq ivy-height 10)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1))

(req-package avy
  :bind (("C-c :" . avy-goto-char)
         ("C-c '" . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :config (avy-setup-default))

(req-package volatile-highlights
  :config
  (volatile-highlights-mode t))

(req-package iedit)

(req-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(req-package goto-chg
  :commands goto-last-change
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(req-package smartparens
  :ensure t
  :demand t
  :config
  (req-package smartparens-config)
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

(req-package spaceline
 :init (setq powerline-default-separator 'wave)
 :config
 (progn
   (require 'spaceline-config)
   (spaceline-emacs-theme)))

(req-package company
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  :config (global-company-mode))

(req-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(req-package projectile
  :init
  (setq projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "node_modules"))
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode))

(req-package magit
  :commands (magit-status projectile-vc)
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(req-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto))
  :init
  (setq org-confirm-babel-evaluate nil)
  (setq org-startup-indented t)
  (setq org-html-doctype "html5")

  (setq org-log-done 'time)

  (setq org-agenda-files '("~/src/org-mode/"))
  (setq org-capture-templates
        '(("t" "acc TODO task format." entry
           (file "~/src/org-mode/todo.org")
           "* TODO %?
SCHEDULED: %t")))

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

  :config
  (require 'ox-deck)
  (add-to-list 'org-src-lang-modes '("js" . js2))
  (add-to-list 'org-src-lang-modes '("deck-js" . js2))

  (defvar org-babel-default-header-args:deck-js
    '((:results . "html")
      (:exports . "results")))
  (defun org-babel-execute:deck-js (body params)
    (let ((ext-lib (assoc :data-external-libs params)))
      (if ext-lib
          (format "<code class=\"javascript\" data-external-libs=\"%s\">\n%s\n</code>" (cdr ext-lib) body)
          (format "<code class=\"javascript\">\n%s\n</code>" body))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (awk . t)
     (sed . t)
     (shell . t)
     (js . t)
     (python . t))))

(req-package ox-reveal
  :require org)

(req-package org-bullets
  :require org
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(req-package clojure-mode
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.cljc$" . clojurec-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(req-package cider
  :commands (cider cider-connect cider-jack-in)
  :pin melpa-stable)

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
  ;(load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq slime-contribs '(slime-fancy
                         slime-indentation
                         slime-compiler-notes-tree))

  ;; CLHS
  ;(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
  )

(req-package haskell-mode
  :mode "\\.hs\\'"
  :init (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(req-package python-mode
  :mode "\\.py\\'"
  :init (setq python-shell-interpreter "python3"))

(req-package ensime
  :commands ensime
  :pin melpa-stable)

(req-package jedi
  :require python-mode
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
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path +rustc-src+)
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode))

(req-package company-racer
  :require (racer company)
  :config
  (add-to-list 'company-backend 'company-racer))

;; Setting up configurations when you load rust-mode
(req-package rust-mode
  :require (company-racer flycheck)
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t)
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

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
  :mode "\\.json\\'")

(req-package reftex
  :require auctex
  :defer t
  :init (setq reftex-plug-into-AUCTeX t))

(req-package prettier-js)

(req-package js2-mode
  :require prettier-js
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c M-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  (define-key js-mode-map (kbd "M-.") nil)

  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

  (add-hook 'js2-mode-hook #'prettier-js-mode))

(req-package web-mode
  :mode "\\.tsx\\'"
  :init
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(req-package rjsx-mode
  :mode "\\.jsx\\'")

(req-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

;; Set up the basic Elixir mode.
(req-package elixir-mode
  :commands elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

;; Alchemist offers integration with the Mix tool.
(req-package alchemist
  :commands alchemist-mode
  :init
  (setq alchemist-goto-elixir-source-dir "/home/acc/src/upstream/elixir")
  (setq alchemist-goto-erlang-source-dir "/home/acc/src/upstream/otp_src_19.2")
  :config
  ;; Bind some Alchemist commands to more commonly used keys.
  (bind-keys :map alchemist-mode-map
             ("C-c C-l" . (lambda () (interactive)
                            (save-buffer)
                            (alchemist-iex-compile-this-buffer))))
  (bind-keys :map alchemist-mode-map
             ("C-x C-e" . alchemist-iex-send-current-line)))

;;; Typescript
(req-package tide
  :require (flycheck company web-mode)
  :preface (defun setup-tide-mode ()
             (interactive)
             (message "Setting up tide mode")
             (tide-setup)
             (turn-on-eldoc-mode)
;             (tide-hl-identifier-mode +1)
             )
  :init
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))

  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (add-hook 'tide-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'tide-format-before-save)))

  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(req-package-finish)

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

(defun acc/swap-quotes ()
  "Swap the quote symbols in a \\[python-mode] string."
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
