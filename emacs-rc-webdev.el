;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nXML Setup

(load "~/.emacs.d/nxml-mode/rng-auto.el")

(dolist (i '("xml" "xsd" "rng" "xsl" "xslt" "svg" "rss"))
  (add-to-list 'auto-mode-alist (cons (concat "\\." i "\\'") 'nxml-mode)))

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

(defun nxml-hippie-try-expand (first-run)
  (run-hook-with-args-until-success 'nxml-completion-hook))

(add-hook 'nxml-mode-hook
          (lambda()
            (nxml-hs-minor-mode t)
            (setq nxml-child-indent 2)
            (setq nxml-auto-insert-xml-declaration-flag t)
            (setq nxml-slash-auto-complete-flag t)
            (set
             (make-local-variable 'hippie-expand-try-functions-list)
             (cons
              'yas/hippie-try-expand
              (cons 'nxml-hippie-try-expand
                    (cdr hippie-expand-try-functions-list))))
            (local-set-key (kbd "<return>") 'newline-and-indent)
            (define-abbrev nxml-mode-abbrev-table "table" ""
              '(lambda() (snippet-insert "<table>$.</table>")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Apache Setup

(autoload 'apache-mode "apache-mode" nil t)
(dolist (i '("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'"
             "access\\.conf\\'" "sites-\\(available\\|enabled\\)/"))
  (add-to-list 'auto-mode-alist (cons i  'apache-mode)))

(add-hook 'apache-mode-hook
          (lambda()
            (set (make-local-variable 'apache-indent-level) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PHP, JavaScript, CSS Setup, YAML

(autoload 'php-mode          "php-mode" "PHP editing mode." t)
(autoload 'php-electric-mode "php-electric" "PHP electric mode." t)
(autoload 'php-flymake-load  "php-flymake" "PHP flymake mode." t)
(autoload 'php-find-function-prototype "php-functions" "PHP functions" t)
(autoload 'yaml-mode         "yaml-mode" "YAML editing mode." t)
(autoload 'css-mode          "css-mode" "Mode for editing CSS files" t)
(autoload 'js2-mode          "js2" "Mode for editing Javascript files" t)

(setq auto-mode-alist  (cons '("\\.php$" . php-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.css$" . css-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.js$" .  js2-mode) auto-mode-alist))
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
;;
;; HTML/mmm-mode setup

(setq load-path (cons (expand-file-name "~/.emacs.d/mmm-mode") load-path))
(require 'mmm-auto)

;; (setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(setq mmm-global-classes nil)

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
    :submode js2-mode
    :face mmm-code-submode-face
    :front "<script[^>]*type=\"text/javascript\"[^>]*"
    :back "</script>")

   (html-css-embeded
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*type=\"text/css\"[^>]*>"
    :back "</style>")))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode ".*" fancy-html))
