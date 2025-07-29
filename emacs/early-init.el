;;; init.el --- Initialization -*- lexical-binding: t; -*-
;;; Commentary:
;;  acc's early-init.el file

;;; Code:
;; Disable garbage collection for startup
(setq gc-cons-threshold most-positive-fixnum)

(setq package-enable-at-startup nil)

;; Disable some graphical stuff early
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(add-to-list 'default-frame-alist '(undecorated . t))

;; Do not resize at startup based on the font size
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
;; suppress the vanilla startup screen completely. we've disabled it with
;; `inhibit-startup-screen', but it would still initialize anyway.
(advice-add #'display-startup-screen :override #'ignore)

(setq initial-scratch-message ";; Scratch\n\n")

(setq window-resize-pixelwise t)

;; Start in `fundamental-mode' to reduce the amount of hooks
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

;; disable any form of bell
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(provide 'early-init)
;;; early-init.el ends here
