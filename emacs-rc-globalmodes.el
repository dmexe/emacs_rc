(require 'speedbar)

(eval-when-compile
  (require 'dired)
  (require 'ispell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case system-type
  ('darwin
   (server-start))
  (t
   (server-mode)
   ; don't ask before kill a client buffer
   (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (eq system-type 'darwin)
  (desktop-save-mode 1)
  (setq desktop-dirname (expand-file-name "~/.emacs.d/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Lock Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Header Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-header)
(header-set-entry "datetime" "$DateTime:$" "")
(header-set-entry "pauthor" "$Author:$" "")
(header-set-entry "headurl" "$HeadURL:$" "")
(header-set-entry "cvsid" "$Id:$" "")
(setq header-field-list (quote (filename author created modified modified_by cvsid)))
(add-to-list 'header-comment-strings '(php-mode . ("/*"   "*/"    " *"   "*")))
(setq header-line-width 85)
(setq header-full-name user-full-name)
(setq header-email-address user-mail-address)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SpeedBar Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq speedbar-frame-parameters
      '((minibuffer)
        (width          . 30)
        (height         . 35)
        (border-width   . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (unsplittable   . t)
        (background-color . "white")))

(setq speedbar-indentation-width 2)

(add-hook 'speedbar-mode-hook
          (lambda()
            (local-set-key (kbd "<DEL>") 'speedbar-up-directory)
            (set (make-local-variable 'speedbar-show-unknown-files) t)))
(add-hook 'speebar-after-create-hook
          (lambda ()
            (set-background-color "white")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iBuffer Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'ibuffer-mode-hook
          (lambda()
            (local-set-key (kbd "RET") 'ibuffer-do-view-other-frame)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require 'tramp)
;; (setq tramp-backup-directory-alist backup-directory-alist)
;; (setq tramp-auto-save-directory backup-directory-alist)
;(setq tramp-verbose 1)
;(setq tramp-default-method "plink")
;; (setq tramp-debug-buffer t)
;; (tramp-set-completion-function "ftp"
;;                                '((tramp-parse-netrc "~/.netrc"))
;;                                )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'dired-load-hook ; dired setup to use single buffer
          (lambda()
            (require 'dired-single)
            (set (make-local-variable 'dired-listing-switches) "-alF")
            (set (make-local-variable 'dired-recursive-deletes) t)
            (set (make-local-variable 'ls-lisp-dirs-first) t)
            (define-key dired-mode-map [return] 'joc-dired-single-buffer)
            (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
            (define-key dired-mode-map (kbd "<DEL>")
              #'(lambda () (interactive) (joc-dired-single-buffer "..")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-Insert Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.templates/")
(setq auto-insert-query nil)
(setq auto-insert-alist
      '(
        (("\\.tex\\'"                          . "LaTeX") . "latex.inc")
        (("\\.php\\'"                          . "PHP") . "php.inc")
        (("[Mm]akefile\\'" . "Makefile")       . "makefile.inc")
        (("\\.\\([Hh]\\|hh\\|hpp\\)\\'"        . "C/C++ header")
         (upcase (concat "_"(file-name-nondirectory
                          (substring buffer-file-name 0 (match-beginning 0)))
                         "_"
                         (substring buffer-file-name (1+ (match-beginning 0))) "_ 1"))
         "#ifndef " str \n
         "#define " str "\n\n"
         _ "\n\n#endif" " /* " str " */")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie-Expand Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(ido-mode t)
(add-to-list 'ido-ignore-buffers "\\*")
(add-hook 'ido-setup-hook
          #'(lambda ()
              (define-key ido-completion-map "\t" 'ido-exit-minibuffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ispell Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(case system-type
  ('darwin
   nil)
  (t
   (setq ispell-process-directory (expand-file-name "D:/local/Aspell/"))
   (setq ispell-program-name "D:/local/Aspell/bin/aspell")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMENU Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq imenu-use-popup-menu t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; predictive Setup
;;;*;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq load-path (cons (expand-file-name "~/.emacs.d/predictive") load-path))
;; (autoload 'predictive-mode "predictive" "Toggle Predictive Completion mode." t)
;; (setq completion-use-echo nil)
;; (setq predictive-use-auto-learn-cache t)
;; (setq predictive-dict-autosave nil)
;; (setq predictive-dict-autosave-on-kill-buffer nil)
;; (add-hook 'text-mode-hook 'predictive-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Untabify Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'untabify-file)
