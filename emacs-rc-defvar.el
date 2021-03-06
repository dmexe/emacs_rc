;; -*- coding: utf-8-unix; -*-

;;; ---------------------------------------------------------
;;; - Email
;;;
(setq user-mail-address "dima.exe@gmail.com")
(setq user-full-name "Dmitry Galinsky")


;;; ---------------------------------------------------------
;;; - Window title format
;;;
(setq-default
 frame-title-format
 (list
  '((buffer-file-name
     " %f"
     (dired-directory
      dired-directory
      (revert-buffer-function
       " %b"
       ("%b - Dir:  " default-directory)))))))


;;; ---------------------------------------------------------
;;; - Display setup
;;;
(tool-bar-mode 0)
(menu-bar-mode 1)
(fringe-mode 0)
(setq-default cursor-type 'hollow)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; ---------------------------------------------------------
;;; - HIghtlight current line
;;;

;; (require 'highline)

(require 'highlight-current-line)
(highlight-current-line-on t)

;; (require 'hl-line)
;; (global-hl-line-mode t)
;; (set-face-background 'hl-line "#202020")


;;; ---------------------------------------------------------
;;; - Default variables
;;;
(setq inhibit-startup-message t) ;; don't display startup message
(setq default-tab-width 2)
(setq c-basic-offset 2)
(setq emacs-lisp-indent-offset 2)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq compilation-scroll-output t)
(setq truncate-lines t)
(column-number-mode 1)
(auto-compression-mode t)
(setq emacsw32-style-frame-title nil)

;;; ---------------------------------------------------------
;;; - Enabled up/lower string convert
;;;
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;; ---------------------------------------------------------
;;; - Delete selection (from pc-selection)
;;;
(delete-selection-mode 1)


;;; ---------------------------------------------------------
;;; - Scrolling setup
;;;
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)
(setq scroll-margin 5)


;;; ---------------------------------------------------------
;;; - Enable auto-revert files
;;;
(global-auto-revert-mode t)


;;; ---------------------------------------------------------
;;; - Auto-save path & backup
;;;
(setq make-backup-files t)
(setq version-control t)
(setq delete-old-versions t)
(add-to-list 'backup-directory-alist
             (cons ".*" "~/.emacs.d/backups/"))

;;; ---------------------------------------------------------
;;; - Setup grep for Windows
;;;
(unless (or (boundp 'grep-find-command)
            (boundp 'grep-find-template))
  (defvar grep-find-command nil "fake defination")
  (defvar grep-find-template nil "fake defination"))
(when (eq system-type 'windows-nt)
  (setq grep-find-command '("gfind . -type f -exec grep -nH -e  {} NUL \";\"" . 34))
  (setq grep-find-template "gfind . <X> -type f <F> -exec grep <C> -nH -e <R> {} NUL \";\""))

;;; ---------------------------------------------------------
;;; - Cocoa Emacs
;;;
(when (eq window-system 'ns)
  (setq ns-expand-space -0.40)
  (setq ns-use-system-highlight-color nil)
  (setq ns-extended-platform-support-mode t))
