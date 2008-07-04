;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load align library

(require 'align)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C Indenting Styles

(c-add-style "microsoft"
             '("ellemtel"
               (c-basic-offset . 2)
               (c-offsets-alist
                (innamespace . 0)
                (case-label . 0)
                (statement-case-intro . +))))

(setq c-default-style "microsoft")
(setq align-c++-modes (quote (c++-mode c-mode java-mode php-mode cperl-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Lisp Setup

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (hs-minor-mode)
            (imenu-add-to-menubar "IMENU")
            (set
             (make-local-variable 'hippie-expand-try-functions-list)
             (cons
              'yas/hippie-try-expand
              (cons 'try-complete-lisp-symbol
                    (cdr hippie-expand-try-functions-list))))
            (local-set-key (kbd "<return>") 'newline-and-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tex/LaTeX Setup

(setq load-path (cons (expand-file-name "~/.emacs.d/auctex") load-path))
(require 'tex-site)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; C/C++ Setup

(defun my-c-mode-common-hook ()
  (imenu-add-to-menubar "IMENU")
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'c-basic-offset) 2)
  (local-set-key (kbd "<return>") 'my-javadoc-return)
  (local-set-key [f7] 'compile)
  (local-set-key (kbd "C-t") 'switch-cpp-h)
  (hs-minor-mode t))

(add-hook 'c-mode-hook (lambda ()
                         (my-c-mode-common-hook)
                         (cwarn-mode)))

(add-hook 'c++-mode-hook (lambda ()
                           (my-c-mode-common-hook)
                           (cwarn-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Perl Setup

(setq auto-mode-alist  (cons '("\\.p\\(l\||lx\\|m\\)$" . cperl-mode) auto-mode-alist))
(add-hook 'cperl-mode-hook
          (lambda ()
            (setq perl-indent-level 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lua Setup

(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'"   . lua-mode))
(add-hook 'lua-mode-hook
          (lambda()
            (set (make-local-variable 'lua-default-application) "d:/local/bin/lua.exe")
            (set (make-local-variable 'lua-indent-level) default-tab-width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assembler Setup

(add-to-list 'auto-mode-alist '("\\.asm\\'"  . asm-mode))
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . asm-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NullSoft Installer Setup

(autoload 'nsi-mode "nsi-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nsi\\'"   . nsi-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Textile Setup

(autoload 'textile-mode "textile-mode" nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SQL Setup

(add-hook 'sql-mode-hook
          (lambda()
            (setq sql-mysql-options '("-C" "-t" "-f" "-n"))
            (setq sql-user "root")
            (setq sql-password "root")
            (add-to-list 'sql-imenu-generic-expression
                         '("Custom" "^-- =\\(\\.*\\)=$" 1))
            (setq imenu-generic-expression sql-imenu-generic-expression
                  imenu-case-fold-search t)
            (imenu-add-to-menubar "IMENU")
            (local-set-key (kbd "<return>") 'newline-and-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subversion Setup

(setq load-path (cons (expand-file-name "~/.emacs.d/svn") load-path))
(require 'psvn)
(add-to-list 'vc-handled-backends 'SVN)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; GIT Setup

(setq load-path (cons (expand-file-name "~/.emacs.d/git") load-path))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Flymake setup

(require 'flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hidshow setup

(defun my/display-code-line-count-in-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ... %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

(add-hook 'hs-minor-mode-hook
          (lambda ()
            (setq hs-set-up-overlay 'my/display-code-line-count-in-overlay)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YASnippet

(setq load-path (cons (expand-file-name "~/.emacs.d/yasnippet") load-path))
(require 'yasnippet-bundle)

;; (require 'yasnippet)

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

;; (yas/initialize)
;; (yas/load-directory "~/.emacs.d/yasnippet/snippets/text-mode")

(defun my/compile-yasnippets ()
  (interactive)
  (yas/compile-bundle "~/.emacs.d/yasnippet/yasnippet.el"
                    "~/.emacs.d/yasnippet/yasnippet-bundle.el"
                    "~/.emacs.d/rc/snippets/"
                    "(yas/initialize)")
  (byte-compile-file "~/.emacs.d/yasnippet/yasnippet-bundle.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Pabbrev setup

(autoload 'pabbrev-mode "pabbrev")
(setq pabbrev-idle-timer-verbose nil)
(dolist (mode '(ruby-mode-hook
                ;emacs-lisp-mode-hook
                php-mode-hook
                apache-mode-hook))
  (add-hook mode (lambda () (pabbrev-mode t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tab completion setup

(dolist (mode '(emacs-lisp-mode-hook
                ruby-mode-hook
                php-mode-hook
                apache-mode-hook))
  (add-hook mode (lambda ()
                    (local-set-key (kbd "<tab>") 'indent-for-tab-command))))
