;; -*- coding: utf-8-unix; -*-

(require 'speedbar)

(eval-when-compile
  (require 'dired)
  (require 'ispell))


;;; ---------------------------------------------------------
;;; - Emacs Server
;;;
(case system-type
  ('darwin
   (server-start))
  (t
   (server-mode)
   ; don't ask before kill a client buffer
   (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))


;;; ---------------------------------------------------------
;;; - Font Lock Setup
;;;
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(transient-mark-mode t)


;;; ---------------------------------------------------------
;;; - Save buffer position
;;;
(require 'saveplace)
(set-default 'save-place t)


;;; ---------------------------------------------------------
;;; - Recent files
;;;
(require 'recentf)
(recentf-mode 1)


;;; ---------------------------------------------------------
;;; - Speedbar setup
;;;
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
          '(lambda()
            (local-set-key (kbd "<DEL>") 'speedbar-up-directory)
            (set (make-local-variable 'speedbar-show-unknown-files) t)))
(add-hook 'speebar-after-create-hook
          '(lambda ()
            (set-background-color "white")))


;;; ---------------------------------------------------------
;;; - iBuffer setup
;;;
(add-hook 'ibuffer-mode-hook
          '(lambda()
            (local-set-key (kbd "RET") 'ibuffer-do-view-other-frame)))


;;; ---------------------------------------------------------
;;; - Dired setup
;;;
(require 'dired-single)
(add-hook 'dired-load-hook ; dired setup to use single buffer
          '(lambda()
             (set (make-local-variable 'dired-listing-switches) "-alF")
             (set (make-local-variable 'dired-recursive-deletes) t)
             (set (make-local-variable 'ls-lisp-dirs-first) t)
             (define-key dired-mode-map [return] 'joc-dired-single-buffer)
             (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
             (define-key dired-mode-map (kbd "<DEL>")
               #'(lambda () (interactive) (joc-dired-single-buffer "..")))))


;;; ---------------------------------------------------------
;;; - Auto-insert setup
;;;
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


;;; ---------------------------------------------------------
;;; - Hippie-expand setup
;;;
(setq hippie-expand-try-functions-list
      '(
        ;try-complete-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers))


;;; ---------------------------------------------------------
;;; - ido setup
;;;
(require 'ido)
(ido-mode t)
(add-to-list 'ido-ignore-buffers "\\*")
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook
          #'(lambda ()
              (define-key ido-completion-map [tab] 'ido-complete)))


;;; ---------------------------------------------------------
;;; - Ispell setup
;;;
(case system-type
  ('darwin
   nil)
  (t
   (setq ispell-process-directory (expand-file-name "D:/local/Aspell/"))
   (setq ispell-program-name "D:/local/Aspell/bin/aspell")))


;;; ---------------------------------------------------------
;;; - imenu setup
;;;
(setq imenu-use-popup-menu t)


;;; ---------------------------------------------------------
;;; - Untabify setup
;;;
(require 'untabify-file)
(add-to-list 'untabify-exclude-list 'ruby-mode)

;;; ---------------------------------------------------------
;;; - Uniqufy setup
;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; ---------------------------------------------------------
;;; - install elisp
;;;
(require 'install-elisp)

;;; ---------------------------------------------------------
;;; - anything
;;;

;; to disable adaptive-history
;; (condition-case nil
;;     (delete-file "~/.emacs.d/anything-c-adaptive-history")
;;   (error nil))

;; (install-elisp-from-emacswiki "anything.el")
(require 'anything)

;; (install-elisp-from-emacswiki "anything-config.el")
(require 'anything-config)

;; (install-elisp "http://www4.atpages.jp/loveloveelisp/anything-c-imenu.el")
(require 'anything-c-imenu)

(setq anything-candidate-number-limit 25)

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(defvar anything-current-buffer nil)
(defadvice anything (before get-current-buffer activate)
  (setq anything-current-buffer (current-buffer)))

(setq anything-c-source-rake-task
  '((name . "Rake Task")
    (candidates
     . (lambda ()
         (when (string-match "^rake" anything-pattern)
           (cons '("rake" . "rake")
                 (mapcar (lambda (line)
                           (cons line (car (split-string line " +#"))))
                         (with-current-buffer anything-current-buffer
                           (split-string (shell-command-to-string "rake -T") "\n" t)))))))
    (action ("Compile" . compile)
            ("Compile with command-line edit"
             . (lambda (c) (let ((compile-command (concat c " ")))
                             (call-interactively 'compile)))))
    (requires-pattern . 4)))

(setq anything-sources
      (list
;;       anything-c-source-rake-task
       anything-c-source-occur
;;       anything-c-source-imenu
       anything-c-source-file-name-history
;;;        anything-c-source-emacs-commands
;;       anything-c-source-complex-command-history
       anything-c-source-locate
       ))

;;; ---------------------------------------------------------
;;; - Path for ruby-mode.el to generate correct imenu index
;;;
;;;  in `ruby-imenu-create-index-in-block' replace:
;;;    (setq pos (match-beginning 0)))
;;;  with:
;;;    (save-excursion
;;;      (goto-char (match-beginning 0))
;;;      (setq pos
;;;        (if imenu-use-markers (point-marker) (point))))
