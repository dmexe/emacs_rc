;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Environment CP1251
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'windows-nt)
  (codepage-setup 866)
  (codepage-setup 1251)

  (set-language-info-alist "Cyrillic-CP1251"
                           '((charset cyrillic-iso8859-5)
                             (coding-system cp1251)
                             (coding-priority cp1251)
                             (input-method . "russian-computer")
                             (features cyril-util)
                             (unibyte-display . cp1251)
                             (sample-text . "Russian Привет!")
                             (documentation . "Support for Cyrillic CP1251."))
                           '("Cyrillic")))

(define-coding-system-alias 'windows-1251     'cp1251)
(define-coding-system-alias 'microsoft-1251   'cp1251)
(define-coding-system-alias 'microsoft-cp1251 'cp1251)
(define-coding-system-alias 'windows-cp1251   'cp1251)

(case system-type
  ('windows-nt
   (set-language-environment 'Cyrillic-CP1251))
  (t
   (set-language-environment "Russian")
   (set-default-coding-systems 'utf-8)
   (set-terminal-coding-system 'utf-8-unix)
   (prefer-coding-system       'utf-8-unix)
   (unless (getenv "LANG")
     (setenv "LANG" "ru_RU.UTF8"))))
