;; -*- coding: utf-8-unix; -*-

(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/local/bin" t))

(require 'cl)

(load "emacs-rc-defvar")
(load "emacs-rc-langenv")
(load "emacs-rc-color-theme")
(load "emacs-rc-functions")
(load "emacs-rc-globalmodes")
(load "emacs-rc-devel")
(load "emacs-rc-webdev")
(load "emacs-rc-ruby")
(load "emacs-rc-compile")
(load "emacs-rc-keys")
