;; -*- coding: utf-8-unix; -*-

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
;; (desktop-save-mode 1)
;; (setq desktop-dirname (expand-file-name "~/.emacs.d/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Lock Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode 1)
(transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save buffer position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'saveplace)
(set-default 'save-place t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'recentf)
(recentf-mode 1)

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
      '(
        ;try-complete-abbrev
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ido)
(ido-mode t)
(add-to-list 'ido-ignore-buffers "\\*")
(setq ido-enable-flex-matching t)
(add-hook 'ido-setup-hook
          #'(lambda ()
              (define-key ido-completion-map [tab] 'ido-complete)))


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
;; Untabify Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'untabify-file nil t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniqufy Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation pasted region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice yank-pop (after indent-region activate)
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode lisp-mode
                           c-mode c++-mode objc-mode
                           latex-mode plain-tex-mode
                           php-mode nxml-mode
                           ruby-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent modification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/yasnippet-p (ov)
  (overlay-get ov 'yas/snippet))

(defun my/do-inside-yasnippet-p ()
  (when (or (find-if 'my/yasnippet-p (overlays-at (point)))
            (find-if 'my/yasnippet-p (overlays-at (- (point) 1))))
    (yas/next-field-group)
    t))

(defadvice indent-for-tab-command (around indent-and-complete activate)
  ;; indent region
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    ;; skip if in snippet
    (unless (my/do-inside-yasnippet-p)
      ;; completing
      (when (looking-at "\\_>")
        (hippie-expand nil))
      ;; always indent line
      ad-do-it)))

;;(ad-activate 'indent-according-to-mode)
