(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH"))))

(require 'cl)

(load-library "emacs-rc-defvar")
(load-library "emacs-rc-color-theme")
(load-library "emacs-rc-langenv")
(load-library "emacs-rc-functions")
(load-library "emacs-rc-globalmodes")
(load-library "emacs-rc-devel")
(load-library "emacs-rc-compile")
(load-library "emacs-rc-keys")

(when (fboundp 'yas/define-snippets)
  (load-library "emacs-rc-yasnippets.el"))
