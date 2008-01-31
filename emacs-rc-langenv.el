;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq w32-use-w32-font-dialog nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Frame Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-default-font "-outline-Consolas-normal-r-normal-normal-13-*-96-96-c-*-iso10646-1")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Environment CP1251
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(codepage-setup 866)
(codepage-setup 1251)

(set-language-info-alist "Cyrillic-CP1251"
                         '((charset cyrillic-iso8859-5)
                          (coding-system cp1251)
                          (coding-priority cp1251)
                          (input-method . "russian-computer")
                          (features cyril-util)
                          (unibyte-display . cp1251)
                          (sample-text . "Russian (Русский)    Здравствуйте!")
                          (documentation . "Support for Cyrillic CP1251."))
                         '("Cyrillic"))

(define-coding-system-alias 'windows-1251 'cp1251)
(define-coding-system-alias 'microsoft-1251 'cp1251)
(define-coding-system-alias 'microsoft-cp1251 'cp1251)
(define-coding-system-alias 'windows-cp1251 'cp1251)

(set-language-environment 'Cyrillic-CP1251)
