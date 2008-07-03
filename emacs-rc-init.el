(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))

(require 'cl)

(load "emacs-rc-defvar")
(load "emacs-rc-color-theme")
(load "emacs-rc-langenv")
(load "emacs-rc-functions")
(load "emacs-rc-globalmodes")
(load "emacs-rc-devel")
(load "emacs-rc-webdev")
(load "emacs-rc-ruby")
(load "emacs-rc-compile")
(load "emacs-rc-keys")

(when (fboundp 'yas/define-snippets)
  (load "emacs-rc-yasnippets.el"))
