;;; .emacs --- acc .emacs file
;;; Commentary:
;;  acc .emacs file

;;; Code:
;;; GC every 20MB allocated (instead of the default 0.76MB)
;(setq +old-gc-cons-threshold+ gc-cons-threshold)
;(setq gc-cons-threshold (* 20 1024 1024))
(add-hook 'focus-out-hook 'garbage-collect)

(defconst +rustc-src+ "/home/acc/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")


(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file :noerror)

;;; Packages
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(require 'package)
(when (version< emacs-version "27.1")
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;;; font selection
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "Monaco-16")
                                        ;  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 138)
  (set-face-attribute 'default nil :font "Iosevka" :height 143))

;;;; emacs confirm closing
(setq confirm-kill-emacs 'yes-or-no-p)

;;;; period single space ends sentence
(setq sentence-end-double-space nil)

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
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;;; Req packages
;; (use-package gruvbox-theme
;;   :ensure t
;;   :config
;;   (load-theme 'gruvbox t))

(use-package zerodark-theme
  :ensure t
  :config
  (load-theme 'zerodark t))


(use-package neotree
  :ensure t
  :bind (("<f8>" . neotree-toggle))
  :init
  (setq neo-theme 'arrow)
  (setq neo-show-hidden-files t)
  (setq neo-smart-open t))

(use-package winner
  :bind (("C-c <right>" . winner-redo)
         ("C-c <left>" . winner-undo))
  :config (winner-mode 1))

(use-package uniquify
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package better-shell
  :ensure t
  :commands (better-shell-shell better-shell-remote-open))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-lighter "#")
  :config
  (minions-mode 1))

(use-package doom-modeline
  :pin melpa-stable
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-major-mode-icon nil))

(use-package which-key
  :ensure t
  :config (which-key-mode))

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
  :bind ("M-o" . ace-window))

;; hydra
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-flymake (global-map "C-c f")
    "flymake"
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("q" nil)))

;; counsel
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-s" . counsel-grep-or-swiper)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)))

(use-package counsel-projectile
  :after (counsel projectile)
  :ensure t
  :config (counsel-projectile-mode 1))

;; ivy
(use-package ivy
  :ensure t
  :demand t
  :after (counsel)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-height 10)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t . ivy--regex-ignore-order)))
  (setq counsel-grep-base-command
        "rg -i -M 200 --no-heading --line-number --color never '%s' %s")

  ;; Make the prompt selectable with <up>
  (setq ivy-use-selectable-prompt t)

  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy
  :ensure t
  :after (ivy)
  :config
  (all-the-icons-ivy-setup))

(use-package ivy-hydra
  :ensure t
  :after (ivy))

(use-package avy
  :ensure t
  :bind (("C-'" . avy-goto-char)
         ("C-c '" . avy-goto-char-2)
         ("M-g f" . avy-goto-line))
  :config (avy-setup-default))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package fic-mode
  :ensure t
  :commands fic-mode
  :hook (prog-mode))

(use-package iedit
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))

(use-package goto-chg
  :ensure t
  :commands goto-last-change
  :bind (("C-." . goto-last-change)
         ("C-," . goto-last-change-reverse)))

(use-package smartparens
  :ensure t
  :demand t
  :config
  (use-package smartparens-config)
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
  :ensure t
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package company-quickhelp
  :after (company)
  :ensure t
  :bind (:map company-active-map
              ("C-c h" . #'company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode 1))

(use-package detour
  :ensure t
  :bind (("s-." . detour-mark)
         ("s-," . detour-back)))

(use-package company-lsp
  :ensure t
  :after (company)

  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode))

(use-package hl-anything
  :ensure t
  :commands hl-highlight-mode
  :init
  (global-set-key (kbd "<f7> <f7>") 'hl-highlight-thingatpt-local)
  (global-set-key (kbd "<f7> u") 'hl-unhighlight-all-local)
  (global-set-key (kbd "<f7> U") 'hl-unhighlight-all-global)
  (global-set-key (kbd "<f7> n") 'hl-find-next-thing)
  (global-set-key (kbd "<f7> p") 'hl-find-prev-thing))

(use-package rainbow-mode
  :ensure t
  :commands rainbow-mode)

(use-package rainbow-delimiters
  :ensure t
  :commands rainbow-delimiters-mode)

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (setq projectile-globally-ignored-directories
        '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "node_modules"))
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  ;(setq vc-handled-backends '(RCS CVS SVN SCCS SRC Bzr Hg Mtn))
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package forge
  :ensure t
  :after magit
  :init
  (setq auth-sources '("~/.authinfo.gpg")))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

(use-package org
  :ensure org-plus-contrib
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

  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELLED")))

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

(use-package ox-reveal
 :ensure t
 :after (org))

(use-package org-bullets
 :ensure t
 :after (org)
 :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode)
         ("\\.cljx$" . clojurex-mode)
         ("\\.cljc$" . clojurec-mode))
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :init
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
  :pin melpa-stable)

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (clojure-mode . paredit-mode)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package ensime
  :ensure t
  :commands ensime
  :pin melpa-stable)

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :pin melpa)

(use-package scala-mode
  :ensure t
  :mode "\\.scala\\'"
  :pin melpa)

(use-package lsp-mode
  :commands lsp
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-update-mode 'point))

(use-package deadgrep
  :ensure t
  :bind ("<f5>" . deadgrep))

;;;; Rust stuff
;; mostly taken from: http://bassam.co/emacs/2015/08/24/rust-with-emacs/
;; Setting up configurations when you load rust-mode
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t))

(use-package flyspell
  :ensure t
  :defer t
  :init
  (setq flyspell-issue-welcome-flag nil)
  (setq-default ispell-program-name "/usr/bin/aspell")
  (setq-default ispell-list-command "list")
  :config
  ;; I don't want flyspell-auto-correct-word bound to C-M-i, C-. is enough.
  (define-key flyspell-mode-map (kbd "C-M-i") nil))

(use-package auctex
  :ensure t
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
  :ensure t
  :after (auctex)
  :init (setq reftex-plug-into-AUCTeX t))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package js2-mode
  :ensure t
  :after (prettier-js)
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (setq js-switch-indent-offset 2)
  (setq js-indent-level 2)
  (setq js2-basic-offset 2)
  :hook (js2-mode . prettier-js-mode)
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c M-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  (define-key js-mode-map (kbd "M-.") nil))

(use-package prettier-js
  :ensure t
  :init
  (setq prettier-js-args '("--trailing-comma" "es5" "--single-quote")))

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'"
  :init
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :after (prettier-js)
  :config
  :hook (rjsx-mode . prettier-js-mode))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

;;; AsciiDoc mode
(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

;;; YAML mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

;;; toml mode
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;;; Groovy
(use-package groovy-mode
  :ensure    t
  :mode      "\\.\\(groovy\\|gradle\\)$")

;; Set up the basic Elixir mode.
(use-package elixir-mode
  :ensure t
  :commands elixir-mode)

;; Alchemist offers integration with the Mix tool.
(use-package alchemist
  :ensure t
  :after (elixir-mode)
  :commands alchemist-mode
  :hook (elixir-mode)
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

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package simple-httpd
  :ensure t
  :commands httpd-start)

;;; Typescript
(use-package tide
  :after (flycheck company js2-mode)
  :init
  (setq typescript-indent-level 2)
  (setq tide-format-options '(:indentSize 2 :tabSize 2))

  :config
  (add-hook 'js2-mode-hook (lambda () (tide-setup)))
  (add-hook 'rjsx-mode-hook (lambda () (tide-setup))))

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
  (while (not (char-equal ?\{ (following-char)))
    (forward-char -1))
  (point))

(defun acc/space-js-object ()
  "Insert one space after the opening `{' and befor the closing `}'."
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

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'acc/smarter-move-beginning-of-line)

(global-set-key (kbd "C-c m s") 'acc/swap-quotes)
(global-set-key (kbd "C-c m a") 'acc/beginning-of-string)
(global-set-key (kbd "C-c m +") 'acc/increment-number-at-point)
(global-set-key (kbd "C-c m {") 'acc/space-js-object)

(provide '.emacs)

(provide 'init)
;;; init ends here
