;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq default-frame-alist
        '((top . 42) (left . 160)
          (width . 120) (height . 40)
          (foreground-color . "white")
          (background-color . "#2B2B2B")
          (font . "-apple-monaco-medium-r-normal--13-160-72-72-m-160-iso10646-1")))
  (setq initial-frame-alist
        '((top . 42) (left . 160)
          (width . 120) (height . 40))))

(case system-type
  ('darwin
   (set-default-font "-apple-monaco-medium-r-normal--13-160-72-72-m-160-iso10646-1"))
  (t
   (set-default-font "-outline-Consolas-normal-r-normal-normal-13-*-96-96-c-*-iso10646-1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Environment CP1251
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(codepage-setup 866)
(codepage-setup 1251)

(when (eq system-type 'windows-nt)
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

(define-coding-system-alias 'windows-1251 'cp1251)
(define-coding-system-alias 'microsoft-1251 'cp1251)
(define-coding-system-alias 'microsoft-cp1251 'cp1251)
(define-coding-system-alias 'windows-cp1251 'cp1251)

(case system-type
  ('windows-nt
   (set-language-environment 'Cyrillic-CP1251))
  (t
   (set-language-environment "Russian")
   (set-default-coding-systems 'utf-8-unix)))
