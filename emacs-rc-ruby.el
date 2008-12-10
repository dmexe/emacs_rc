;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ruby

(setq load-path (cons (expand-file-name "~/.emacs.d/ruby") load-path))
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)
(autoload 'ruby-electric-mode "ruby-electric" "Ruby electric mode." t)

(add-to-list 'auto-mode-alist (cons (concat "\\.rb\\'") 'ruby-mode))
(add-to-list 'auto-mode-alist (cons (concat "Rakefile\\'") 'ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (set (make-local-variable 'tab-width) 2)
            (ruby-electric-mode t)
            (ruby-hs-minor-mode t)
            (imenu-add-to-menubar "IMENU")
            (modify-syntax-entry ?! "w" (syntax-table))
            (modify-syntax-entry ?: "w" (syntax-table))
            (modify-syntax-entry ?_ "w" (syntax-table))
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (kbd "C-:") 'my/ruby-toggle-string<>simbol)
            (local-set-key (kbd "<return>") 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup align for ruby

(require 'align)

(defconst align-ruby-modes '(ruby-mode)
  "align-perl-modes is a variable defined in `align.el'.")

(defconst ruby-align-rules-list
  '((ruby-comma-delimiter
     (regexp . ",\\(\\s-*\\)[^/ \t\n]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-string-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\)['\"]\\w+['\"]")
     (modes  . align-ruby-modes)
     (repeat . t))
    (ruby-symbol-after-func
     (regexp . "^\\s-*[a-zA-Z0-9.:?_]+\\(\\s-+\\):\\w+")
     (modes  . align-ruby-modes)))
  "Alignment rules specific to the ruby mode.
See the variable `align-rules-list' for more details.")

(dolist (it '(align-perl-modes align-dq-string-modes align-sq-string-modes align-open-comment-modes))
  (add-to-list it 'ruby-mode))

(dolist (it ruby-align-rules-list)
  (add-to-list 'align-rules-list it))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup hideshow for ruby

(defun ruby-hs-minor-mode (&optional arg)
  (interactive)
  (require 'hideshow)
  (unless (assoc 'ruby-mode hs-special-modes-alist)
    (setq
     hs-special-modes-alist
     (cons (list 'ruby-mode
                 "\\(def\\|do\\)"
                 "end"
                 "#"
                 (lambda (&rest args) (ruby-end-of-block))
                 ;(lambda (&rest args) (ruby-beginning-of-defun))
                 )
           hs-special-modes-alist)))
  (hs-minor-mode arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setup flymake for ruby

(defconst flymake-allowed-ruby-file-name-masks
  '(("\\.rb\\'"      flymake-ruby-init)
    ("\\.rxml\\'"    flymake-ruby-init)
    ("\\.builder\\'" flymake-ruby-init)
    ("\\.rjs\\'"     flymake-ruby-init))
  "Filename extensions that switch on flymake-ruby mode syntax checks.")

(defconst flymake-ruby-error-line-pattern-regexp
  '("^\\([^:]+\\):\\([0-9]+\\): *\\([\n]+\\)" 1 2 nil 3)
  "Regexp matching ruby error messages.")

(defun flymake-ruby-init ()
  (condition-case er
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file  (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
        (list "ruby" (list "-c" local-file)))
    ('error ())))

(defun flymake-ruby-load ()
  (when (and (buffer-file-name)
             (loop for re in flymake-allowed-ruby-file-name-masks
                   for allow = (string-match (car re) (buffer-file-name))
                   when allow do (return re)))
    (setq flymake-allowed-file-name-masks
          (append flymake-allowed-file-name-masks flymake-allowed-ruby-file-name-masks))
    (setq flymake-err-line-patterns
          (cons flymake-ruby-error-line-pattern-regexp flymake-err-line-patterns))
    (flymake-mode t)))

(when (featurep 'flymake)
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Rails

(setq load-path (cons (expand-file-name "~/.emacs.d/rails-reloaded") load-path))
(require 'rails-autoload)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HAML

(setq load-path (cons (expand-file-name "~/.emacs.d/haml") load-path))
(autoload 'haml-mode "haml-mode" "Major mode to edit HAML files")
(add-to-list 'auto-mode-alist (cons (concat "\\.haml\\'") 'haml-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Ruby functions

(defun my/ruby-toggle-string<>simbol ()
  "Easy to switch between strings and symbols."
  (interactive)
  (let ((initial-pos (point)))
    (save-excursion
      (when (looking-at "[\"']") ;; skip beggining quote
        (goto-char (+ (point) 1))
        (unless (looking-at "\\w")
          (goto-char (- (point) 1))))
      (let* ((point (point))
             (start (skip-syntax-backward "w"))
             (end (skip-syntax-forward "w"))
             (end (+ point start end))
             (start (+ point start))
             (start-quote (- start 1))
             (end-quote (+ end 1))
             (quoted-str (buffer-substring-no-properties start-quote end-quote))
             (symbol-str (buffer-substring-no-properties start end)))
        (cond
         ((string-match "^\'\\w+\'$" quoted-str)
          (setq quoted-str (substring quoted-str 1 (- (length quoted-str) 1)))
          (kill-region start-quote end-quote)
          (goto-char start-quote)
          (insert (concat "\"" quoted-str "\"")))
         ((string-match "^\"\\w+\"$" quoted-str)
          (setq quoted-str (substring quoted-str 1 (- (length quoted-str) 1)))
          (kill-region start-quote end-quote)
          (goto-char start-quote)
          (insert (concat ":" quoted-str)))
         ((string-match "^\:\\w+$" symbol-str)
          (setq symbol-str (substring symbol-str 1))
          (kill-region start end)
          (goto-char start)
          (insert (format "'%s'" symbol-str))))))
    (goto-char initial-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; RHTML with muamo

(add-to-list 'load-path "~/.emacs.d/nxhtml/util")
(require 'mumamo-fun)
(setq mumamo-chunk-coloring 'submode-colored)
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . eruby-html-mumamo))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . eruby-html-mumamo))
