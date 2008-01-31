;; (eval-when-compile
;;  (require 'nxml-mode)
;;   (require 'css-mode)
;;   (require 'lua-mode)
;;   (require 'apache-mode))

(require 'snippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Indenting Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Text-mode setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'text-mode-hook #'(lambda()
                              (local-set-key (kbd "<tab>") 'indent-and-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/nxml-mode/rng-auto.el")
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.sch$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rng$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xslt$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.svg$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.rss$" . nxml-mode))

(defun nxml-indent-and-complete ()
  (interactive)
     ;; hippie-expand
  (when (looking-at "\\_>")
    (flet ((message (format-string &rest args) nil)
           (ding (&optional arg) nil))
      (nxml-complete)))
  (indent-for-tab-command))

(defun nxml-hs-minor-mode (&rest arg)
  (interactive "P")
  (unless (assoc 'nxml-mode hs-special-modes-alist)
    (add-to-list 'hs-special-modes-alist
                 '(nxml-mode
                   "\\|<[^/>]&>\\|<[^/][^>]*[^/]>"
                   ""
                   ""
                   ;; won't work on its own; uses syntax table
                   nxml-forward-element
                   nil)))
  (hs-minor-mode arg))

(add-hook 'nxml-mode-hook
          (lambda()
            (nxml-hs-minor-mode t)
            (setq nxml-child-indent 2)
            (setq nxml-auto-insert-xml-declaration-flag t)
            (setq nxml-slash-auto-complete-flag t)
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (local-set-key (kbd "<tab>") 'nxml-indent-and-complete)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apache Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
(add-hook 'apache-mode-hook
          (lambda()
            (set (make-local-variable 'apache-indent-level) 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (hs-minor-mode)
            (imenu-add-to-menubar "IMENU")
            (set
             (make-local-variable 'hippie-expand-try-functions-list)
             '(try-complete-abbrev
               try-complete-lisp-symbol
               try-expand-dabbrev
               try-expand-dabbrev-all-buffers))
            (local-set-key (kbd "<tab>") 'indent-and-complete)
            (local-set-key (kbd "<return>") 'newline-and-indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tex/LaTeX Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/auctex") load-path))
(require 'tex-site)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-c-mode-common-hook ()
  (imenu-add-to-menubar "IMENU")
  (set (make-local-variable 'indent-tabs-mode) 'nil)
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'c-basic-offset) 2)
  (local-set-key (kbd "<tab>") 'indent-and-complete)
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
;; PHP, JavaScript, CSS Setup, YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'php-mode "php-mode" "PHP editing mode." t)
(autoload 'php-electric-mode "php-electric" "PHP electric mode." t)
(autoload 'php-flymake-load "php-flymake" "PHP flymake mode." t)
(autoload 'php-find-function-prototype "php-functions" "PHP functions" t)
(autoload 'yaml-mode "yaml-mode" "YAML editing mode." t)
(autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
(autoload 'ecmascript-mode "ecmascript-mode" "Mode for editing ECMA Javascript files" t)
(autoload 'javascript-generic-mode "generic-x" "Mode for editing Javascript files" t)
(setq auto-mode-alist  (cons '("\\.php$" . php-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.js$" . ecmascript-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.y[a]?ml$" . yaml-mode) auto-mode-alist))

(add-hook 'php-mode-hook
          (lambda()
            (php-electric-mode 1)
            (php-flymake-load)
            (my-c-mode-common-hook)
            (modify-syntax-entry ?_ "w" php-mode-syntax-table)
            (local-set-key (kbd "<f1>") (lambda()
                                          (interactive)
                                          (let ((sym (thing-at-point 'sexp)))
                                            (when sym
                                              (browse-url (concat "http://php.net/" sym))))))
            (local-set-key (kbd "<f2>") 'php-find-function-prototype)
            (local-set-key (kbd "<f7>") 'php-lint)
            (local-set-key (kbd "<f12>") 'php-insert-phpdoc)
            (local-set-key (kbd "<C-f12>") 'php-insert-comment)))

(add-hook 'css-mode-hook
          (lambda()
            (setq css-indent-offset 2)
            (setq css-electric-brace-behavior t)
            (setq css-electric-semi-behavior t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons (expand-file-name "~/.emacs.d/rails/trunk") load-path))
(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)

(require 'rails)
(setq rails-use-ctags t)
(setq rails-use-mongrel t)

(add-hook 'ruby-mode-hook
          (lambda()
            (set (make-local-variable 'tab-width) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML/mmm-mode setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/mmm-mode") load-path))
(require 'mmm-auto)
;; (setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(mmm-add-group
 'fancy-html
 '((html-erb
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))

   (html-php-embeded
    :submode php-mode
    :face mmm-code-submode-face
    :front "<\\?\\(\\|php\\|=\\)?"
    :back "\\?>"
    :insert ((?p php-code   nil @ "<?php"  @ " " _ " " @ "?>" @)
             (?P php-print  nil @ "<?=" @ " " _ " " @ "?>" @)))

   (html-js-embeded
    :submode javascript-generic-mode
    :face mmm-code-submode-face
    :front "<script[^>]*type=\"text/javascript\"[^>]*"
    :back "</script>")

;;    (html-js-attribute
;;     :submode javascript-generic-mode
;;     :face mmm-declaration-submode-face
;;     :front "\\bon\\w+=\""
;;     :back "\"")

   (html-css-embeded
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*type=\"text/css\"[^>]*>"
    :back "</style>")

;;    (html-css-attribute
;;     :submode css-mode
;;     :face mmm-declaration-submode-face
;;     :front "\\bstyle=\\s-*\""
;;     :back "\"")
))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil fancy-html))

(add-hook 'sgml-mode-hook
          (lambda()
            (mmm-mode-on)
            (local-set-key (kbd "<tab>") 'indent-and-complete)
            (local-set-key (kbd "<return>") 'newline-and-indent)))

(add-hook 'html-mode-hook
          (lambda()
            (define-abbrev html-mode-abbrev-table "table" ""
              '(lambda() (snippet-insert "<table>$.</table>")))
            (define-abbrev html-mode-abbrev-table "tr" ""
              '(lambda() (snippet-insert "<tr>$.</tr>")))
            (define-abbrev html-mode-abbrev-table "th" ""
              '(lambda() (snippet-insert "<th>$.</th>")))
            (define-abbrev html-mode-abbrev-table "td" ""
              '(lambda() (snippet-insert "<td>$.</td>")))
            (define-abbrev html-mode-abbrev-table "ul" ""
              '(lambda() (snippet-insert "<ul>$.</ul>")))
            (define-abbrev html-mode-abbrev-table "li" ""
              '(lambda() (snippet-insert "<li>$.</li>")))
            (define-abbrev html-mode-abbrev-table "h1" ""
              '(lambda() (snippet-insert "<h1>$.</h1>")))
            (define-abbrev html-mode-abbrev-table "h2" ""
              '(lambda() (snippet-insert "<h2>$.</h2>")))
            (define-abbrev html-mode-abbrev-table "h3" ""
              '(lambda() (snippet-insert "<h3>$.</h3>")))
            (define-abbrev html-mode-abbrev-table "h6" ""
              '(lambda() (snippet-insert "<h6>$.</h6>")))
            (define-abbrev html-mode-abbrev-table "form" ""
              '(lambda() (snippet-insert "<form method=\"$${post}\">$.</form>")))
            (define-abbrev html-mode-abbrev-table "input" ""
              '(lambda() (snippet-insert "<input type=\"$${text}\" name=\"$${name}\" value=\"$${value}\" />")))
            (define-abbrev html-mode-abbrev-table "br" ""
              '(lambda() (snippet-insert "<br />")))
            (define-abbrev html-mode-abbrev-table "label" ""
              '(lambda() (snippet-insert "<label></label>")))
            (define-abbrev html-mode-abbrev-table "div" ""
              '(lambda() (snippet-insert "<div>$.</div>")))
            (define-abbrev html-mode-abbrev-table "span" ""
              '(lambda() (snippet-insert "<span>$.</span>")))
            (define-abbrev html-mode-abbrev-table "a" ""
              '(lambda() (snippet-insert "<a href=\"$${http://}\">$.</a>")))
            (define-abbrev html-mode-abbrev-table "img" ""
              '(lambda() (snippet-insert "<img src=\"$${http://}\">$.</img>")))

            (define-abbrev html-mode-abbrev-table "php" ""
              '(lambda() (snippet-insert "<?php $. ?>")))
            (define-abbrev html-mode-abbrev-table "echo" ""
              '(lambda() (snippet-insert "<?= $. ?>")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua\\'"   . lua-mode))
(add-hook 'lua-mode-hook
          (lambda()
            (set (make-local-variable 'lua-default-application) "d:/local/bin/lua.exe")
            (set (make-local-variable 'lua-indent-level) default-tab-width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assembler Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.asm\\'"  . asm-mode))
(add-to-list 'auto-mode-alist '("\\.nasm\\'" . asm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NullSoft Installer Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'nsi-mode "nsi-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nsi\\'"   . nsi-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Jam Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'jam-mode "jam-mode" nil t)
(add-to-list 'auto-mode-alist '("Jamfile"   . jam-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textile Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'textile-mode "textile-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (autoload 'sql-mode "sql-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.sql\\'"   . sql-mode))
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
            (local-set-key (kbd "<tab>") 'indent-and-complete)
            (local-set-key (kbd "<return>") 'newline-and-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/svn") load-path))
(require 'psvn)
(add-to-list 'vc-handled-backends 'SVN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/git") load-path))
(require 'git)
(require 'vc-git)
(add-to-list 'vc-handled-backends 'GIT)

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haml mode Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path (cons (expand-file-name "~/.emacs.d/haml") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pabbrev setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'pabbrev-mode "pabbrev")
(setq pabbrev-idle-timer-verbose nil)
(dolist (mode '(emacs-lisp-mode-hook
                ruby-mode-hook
                php-mode-hook
                apache-mode-hook
                ruby-mode-hook))
  (add-hook mode (lambda () (pabbrev-mode t))))
