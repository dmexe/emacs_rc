;; -*- coding: utf-8-unix; -*-

(require 'cl)

;;; ---------------------------------------------------------
;;; - Load align
;;;
(require 'align)


;;; ---------------------------------------------------------
;;; - C indenting styles
;;;
(c-add-style "microsoft"
             '("ellemtel"
               (c-basic-offset . 2)
               (c-offsets-alist
                (innamespace . 0)
                (case-label . 0)
                (statement-case-intro . +))))

(setq c-default-style "microsoft")
(setq align-c++-modes (quote (c++-mode c-mode java-mode php-mode cperl-mode)))


;;; ---------------------------------------------------------
;;; - Emacs Lisp Setup
;;;
(defun my/pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\) *("
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun my/emacs-lisp-custom-keywords ()
  (font-lock-add-keywords
   nil
   '(("(\\(when-bind\\|rails/root\\|rails/with-root\\|rails/with-current-buffer\\|rails/defresource\\|rails/defbundle\\)\\>" 1 font-lock-keyword-face))))

(put 'when-bind 'lisp-indent-function 1)
(put 'rails/with-root 'lisp-indent-function 1)
(put 'rails/root 'lisp-indent-function 1)
(put 'rails/defresource 'lisp-indent-function 1)
(put 'rails/defbundle 'lisp-indent-function 1)

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
            (hs-minor-mode)
            (imenu-add-to-menubar "IMENU")
            (my/pretty-lambdas)
            (my/emacs-lisp-custom-keywords)
            (set
             (make-local-variable 'hippie-expand-try-functions-list)
             (cons
              'yas/hippie-try-expand
              (cons 'try-complete-lisp-symbol
                    (cdr hippie-expand-try-functions-list))))
            (local-set-key (kbd "<return>") 'newline-and-indent)))

;;; ---------------------------------------------------------
;;; - C/C++ setup
;;;
(defun my/c-mode-common-hook ()
  (imenu-add-to-menubar "IMENU")
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'c-basic-offset) 2)
  (local-set-key (kbd "<return>") 'my/javadoc-return)
  (local-set-key [f7] 'compile)
  (local-set-key (kbd "C-t") 'switch-cpp-h)
  (hs-minor-mode t))

(add-hook 'c-mode-hook '(lambda ()
                         (my/c-mode-common-hook)
                         (cwarn-mode)))

(add-hook 'c++-mode-hook '(lambda ()
                           (my/c-mode-common-hook)
                           (cwarn-mode)))


;;; ---------------------------------------------------------
;;; - Perl setup
;;;
(setq auto-mode-alist  (cons '("\\.p\\(l\||lx\\|m\\)$" . cperl-mode) auto-mode-alist))
(add-hook 'cperl-mode-hook
          '(lambda ()
            (setq perl-indent-level 2)))


;;; ---------------------------------------------------------
;;; - Lua setup
;;;
(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'"   . lua-mode))
(add-hook 'lua-mode-hook
          '(lambda ()
            (set (make-local-variable 'lua-default-application) "d:/local/bin/lua.exe")
            (set (make-local-variable 'lua-indent-level) default-tab-width)))


;;; ---------------------------------------------------------
;;; - Assembler setup
;;;
(add-to-list 'auto-mode-alist '("\\.asm\\'"  . asm-mode))
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . asm-mode))


;;; ---------------------------------------------------------
;;; - NullSoft installer setup
;;;
(autoload 'nsi-mode "nsi-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nsi\\'"   . nsi-mode))


;;; ---------------------------------------------------------
;;; - Textile setup
;;;
(autoload 'textile-mode "textile-mode" nil t)


;;; ---------------------------------------------------------
;;; - SQL setup
;;;
(add-hook 'sql-mode-hook
          '(lambda()
            (setq sql-mysql-options '("-C" "-t" "-f" "-n"))
            (setq sql-user "root")
            (setq sql-password "root")
            (add-to-list 'sql-imenu-generic-expression
                         '("Custom" "^-- =\\(\\.*\\)=$" 1))
            (setq imenu-generic-expression sql-imenu-generic-expression
                  imenu-case-fold-search t)
            (imenu-add-to-menubar "IMENU")
            (local-set-key (kbd "<return>") 'newline-and-indent)))


;;; ---------------------------------------------------------
;;; - Subversion Setup
;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/svn") load-path))
(require 'psvn)
(add-to-list 'vc-handled-backends 'SVN)


;;; ---------------------------------------------------------
;;; - GIT setup
;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/git") load-path))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)


;;; ---------------------------------------------------------
;;; - Flymake setup
;;;
(require 'flymake)


;;; ---------------------------------------------------------
;;; - Hidshow setup
;;;
(defun my/display-code-line-count-in-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'face 'font-lock-comment-face)
    (overlay-put ov 'display
                 (format " ... %d lines"
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))))

(add-hook 'hs-minor-mode-hook
          '(lambda ()
            (setq hs-set-up-overlay 'my/display-code-line-count-in-overlay)))


;;; ---------------------------------------------------------
;;; - YASnippet
;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/yasnippet") load-path))

(defun my/compile-yasnippets ()
  (interactive)
  (require 'yasnippet)
  (yas/compile-bundle "~/.emacs.d/yasnippet/yasnippet.el"
                      "~/.emacs.d/yasnippet/yasnippet-bundle.el"
                      "~/.emacs.d/rc/snippets/"
                      "(setq-default yas/dont-activate t)(yas/initialize)")
  (byte-compile-file "~/.emacs.d/yasnippet/yasnippet-bundle.el"))

;; (unless (file-exists-p "~/.emacs.d/yasnippet/yasnippet-bundle.el")
;;   (my/compile-yasnippets))

;; (require 'yasnippet-bundle)

(require 'yasnippet)
(setq-default yas/dont-activate t)
(yas/initialize)
(yas/load-directory "~/.emacs.d/rc/snippets")

(setq hippie-expand-try-functions-list
      (cons 'yas/hippie-try-expand hippie-expand-try-functions-list))

;;; ---------------------------------------------------------
;;; - Fold dwim
;;;
(require 'fold-dwim)


;;; ---------------------------------------------------------
;;; - Force tab completion setup
;;;
(defmacro my/setup-indent-for-tab-command (mode)
  `(add-hook ,mode
             '(lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command))))

(dolist (mode '(emacs-lisp-mode-hook
                ruby-mode-hook
                php-mode-hook
                apache-mode-hook))
  (my/setup-indent-for-tab-command mode))


;;; ---------------------------------------------------------
;;; - Automatic indentation pasted region
;;;
(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode lisp-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode
                           php-mode nxml-mode
                           ruby-mode))
      (indent-region (region-beginning) (region-end) nil)))


;;; ---------------------------------------------------------
;;; - Indent modification
;;;
(defun my/yasnippet-p (ov)
  (overlay-get ov 'yas/snippet))

(defun my/do-inside-yasnippet-p ()
  (when (or (find-if 'my/yasnippet-p (overlays-at (point)))
            (find-if 'my/yasnippet-p (overlays-at (- (point) 1))))
    (yas/next-field-group)
    t))

(defadvice indent-for-tab-command (around indent-and-complete activate)
  "Do:
* Indent region if `mark-active'
** (or) Next yasnippet group if inside snippet
*** (or) Complete using `hippie-expand'
* (unless) major-mode have property 'indent-or-complete and before sexps was called
**  ad-do-it"
  (let (run)
    ;; indent region
    (setq
     run
     (if mark-active
         (progn
           (indent-region (region-beginning)
                          (region-end))
           t)
       ;; skip if in snippet
       (if (my/do-inside-yasnippet-p)
           t
         ;; completing
         (when (or
                (looking-at "\\_>")
                (looking-back "\<")) ;; special for html '<' snippet
           (hippie-expand nil)
           t))))
    (unless (and run
                 (get major-mode 'indent-or-complete))
      ;; always indent line
      ad-do-it)))

;;; ---------------------------------------------------------
;;; - Auto run smerge
;;;
(defun my/try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)