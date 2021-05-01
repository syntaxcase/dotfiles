;; Disable garbage collection for startup
(setq gc-cons-threshold most-positive-fixnum)

;; Disable some graphical stuff early
(setq tool-bar-mode nil)
(setq menu-bar-mode nil)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Do not resize at startup based on the font size
(setq frame-inhibit-implied-resize t)
