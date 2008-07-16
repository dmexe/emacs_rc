;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setq default-frame-alist
        '((top . 42) (left . 160)
          (width . 120) (height . 40)
          (foreground-color . "white")
          (background-color . "#202020")
          (font . "-apple-monaco-medium-r-normal--13-160-72-72-m-160-iso10646-1")))
  (setq initial-frame-alist
        '((top . 42) (left . 160)
          (width . 120) (height . 40))))

(case system-type
  (darwin
   (set-default-font "-apple-monaco-medium-r-normal--13-160-72-72-m-160-iso10646-1"))
  (t
   (set-default-font "-outline-Consolas-medium-r-normal-normal-14-*-96-96-c-*-iso10646-1")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default colors left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; based on the idleFingers Textmate theme
(defun color-theme-dimaexe()
  (interactive)
  (custom-set-faces
   '(default ((t (:background "#202020" :foreground "#E5E5E5"))))
   '(match ((t :background "#4A6152")))
   '(cursor ((t (:background "#FFFB7B"))))
   '(region ((t :background "#303C54")))
   '(font-lock-comment-face ((t (:italic t :foreground "#B150E7"))))
   '(font-lock-string-face ((t (:foreground "#A5F26E"))))
   '(font-lock-keyword-face ((t (:foreground "#DC762A")))) ;; #CC7832"
   '(font-lock-warning-face ((t (:underline "red"))))
   '(font-lock-constant-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-type-face ((t (:foreground "#CACE28")))) ;; "#8888ff"
   '(font-lock-variable-name-face ((t (:foreground "#D0D0F3")))) ;;
   '(font-lock-function-name-face ((t (:foreground "#FFFB7B")))) ;; "#E8BF6A"
   '(font-lock-builtin-face ((t (:foreground "#59ACC2"))))
   '(font-lock-preprocessor-face ((t (:background "#282828")))) ;; :foreground "#C9C9C9"))))

   ;; jabber
   '(jabber-roster-user-online ((t (:inherit 'font-lock-string-face))))
   '(jabber-roster-user-away ((t (:inherit 'font-lock-string-face))))
   '(jabber-roster-user-xa ((t (:inherit 'font-lock-string-face))))
   '(jabber-chat-prompt-foreign ((t :inherit 'font-lock-builtin-face)))
   '(jabber-rare-time-face ((t :inherit 'font-lock-keyword-face)))

   ;; Compilation
   '(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
   '(compilation-error ((t (:background "sienna4" :bold t))))
   '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
   '(flymake-errline ((t :underline "red")))
   '(flymake-warnline ((t :underline "green")))

   ;; MMM
   '(mmm-declaration-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-default-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-output-submode-face  ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-code-submode-face    ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-comment-submode-face ((t (:inherit 'font-lock-comment-face))))

   ;; mumamo
   '(mumamo-background-chunk-submode ((t (:inherit 'font-lock-preprocessor-face))))

   ; nXML
   '(nxml-element-colon-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-prefix-face    ((t (:bold t :foreground "#92D229"))))

   '(nxml-attribute-value-delimiter-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-cdata-section-content-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-value-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-entity-ref-name-face ((t (:inherit 'font-lock-constant-face))))

   '(nxml-element-colon-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-prefix-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-local-name-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-delimiter-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-slash-face    ((t (:inherit 'font-lock-function-name-face))))

   '(nxml-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
   '(nxml-comment-content-face ((t (:inherit 'font-lock-comment-face))))

   ; ido
   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))

   ; diff
   '(diff-header ((t (:background "#3A65C2"))))
   '(diff-file-header ((t (:background "#5A85E2"))))
   '(diff-indicator-removed ((t (:background "#EB3A3A"))))
   '(diff-removed ((t (:inherit 'diff-indicator-removed))))
   '(diff-indicator-added ((t (:background "#33AE27" :foreground "white"))))
   '(diff-added ((t (:inherit 'diff-indicator-added))))

   ; js2
   '(js2-external-variable-face ((t (:inherit 'font-lock-variable-name-face))))
   '(js2-function-param-face ((t (:inherit 'font-lock-variable-name-face))))
   '(js2-warning-face ((t (:inherit 'font-lock-warning-face))))
   ; modeline nad minibuffer
   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   '(mode-line ((t (:background "#EFEFEF"
                    :foreground "#333133"
                    :height 0.9
                    :box (:line-width 2 :color "#EEEEEE" :style nil)))))
   '(mode-line-inactive ((t (:inherit 'mode-line :background "#AEAEAE"))))
   '(mode-line-buffer-id ((t (:inherit 'mode-line :bold t))))
   '(mode-line-highlight ((t (:inherit 'mode-line-inactive))))

   '(completion-dynamic-face ((t (:inherit 'match))))
   '(highlight ((t (:inherit 'match))))))


(defun color-theme-vibrant-ink ()
  (interactive)
  (custom-set-faces
   '(default ((t (:background "#000000" :foreground "#FFFFFF"))))
   '(match ((t :background "#CACE28" :foreground "#FFFFFF")))
   '(cursor ((t (:background "#FFFB7B"))))
   '(region ((t :background "#8623B9")))
   '(font-lock-comment-face ((t (:italic t :foreground "#B150E7"))))
   '(font-lock-string-face ((t (:foreground "#00FF00"))))
   '(font-lock-keyword-face ((t (:foreground "#FF6100")))) ;; #CC7832"
   '(font-lock-warning-face ((t (:underline "red"))))
   '(font-lock-constant-face ((t (:foreground "#009B9B"))))
   '(font-lock-type-face ((t (:foreground "#A2A44E")))) ;; "#8888ff"
   '(font-lock-variable-name-face ((t (:foreground "#FFFFFF")))) ;;
   '(font-lock-function-name-face ((t (:foreground "#FFCD00")))) ;; "#E8BF6A"
   '(font-lock-builtin-face ((t (:foreground "#009B9B"))))
   '(font-lock-preprocessor-face ((t (:background "#101010" :foreground "#FFFFFF"))))

   ;; Compilation
   '(compilation-info ((t (:inherit 'font-lock-string-face :bold t))))
   '(compilation-error ((t (:background "sienna4" :bold t))))
   '(compilation-line-number ((t (:foreground "#FF6666" :bold t))))
   '(flymake-errline ((t :underline "red")))
   '(flymake-warnline ((t :underline "green")))

   ;; MMM
   '(mmm-declaration-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-default-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-output-submode-face  ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-code-submode-face    ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-comment-submode-face ((t (:inherit 'font-lock-comment-face))))

   ; nXML
   '(nxml-element-colon-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-prefix-face    ((t (:bold t :foreground "#92D229"))))

   '(nxml-attribute-value-delimiter-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-cdata-section-content-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-value-face ((t (:inherit 'font-lock-string-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-attribute-local-name-face ((t (:inherit 'font-lock-constant-face))))
   '(nxml-entity-ref-name-face ((t (:inherit 'font-lock-constant-face))))

   '(nxml-element-colon-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-prefix-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-element-local-name-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-delimiter-face    ((t (:inherit 'font-lock-function-name-face))))
   '(nxml-tag-slash-face    ((t (:inherit 'font-lock-function-name-face))))

   '(nxml-comment-delimiter-face ((t (:inherit 'font-lock-comment-face))))
   '(nxml-comment-content-face ((t (:inherit 'font-lock-comment-face))))

   ; ido
   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))

   ; diff
   '(diff-header ((t (:background "#3A65C2"))))
   '(diff-file-header ((t (:background "#5A85E2"))))
   '(diff-indicator-removed ((t (:background "#EB3A3A"))))
   '(diff-removed ((t (:inherit 'diff-indicator-removed))))
   '(diff-indicator-added ((t (:background "#33AE27" :foreground "white"))))
   '(diff-added ((t (:inherit 'diff-indicator-added))))

   ; js2
   '(js2-external-variable-face ((t (:inherit 'font-lock-variable-name-face))))
   '(js2-function-param-face ((t (:inherit 'font-lock-variable-name-face))))
   '(js2-warning-face ((t (:inherit 'font-lock-warning-face))))
   ; modeline nad minibuffer
   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   '(mode-line ((t (:background "#EFEFEF"
                    :foreground "#333133"
                    :height 0.9
                    :box (:line-width 2 :color "#EEEEEE" :style nil)))))
   '(mode-line-inactive ((t (:inherit 'mode-line :background "#AEAEAE"))))
   '(mode-line-buffer-id ((t (:inherit 'mode-line :bold t))))
   '(mode-line-highlight ((t (:inherit 'mode-line-inactive))))

;;;    '(tooltip ((t
;;;                (:family
;;;                 "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso10646-1"
;;;                 :height 0.8
;;;                 :foreground "systeminfotext"
;;;                 :background "systeminfowindow"
;;;                 :inherit (variable-pitch)))))

   '(completion-dynamic-face ((t (:inherit 'match))))
   '(highlight ((t (:inherit 'match))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load color-theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(color-theme-dimaexe)
