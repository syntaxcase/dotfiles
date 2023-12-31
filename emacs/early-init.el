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

(setq window-resize-pixelwise t)

;; Start in `fundamental-mode' to reduce the amount of hooks
(setq inhibit-default-init t)
(setq initial-major-mode 'fundamental-mode)

;; disable any form of bell
(setq ring-bell-function #'ignore)
(setq visible-bell nil)

(provide 'early-init)
;;; early-init.el ends here
