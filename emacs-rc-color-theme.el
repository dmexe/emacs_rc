;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default colors left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-theme-my-light ()
  (interactive)
  (color-theme-gtk-ide)
  (custom-set-faces
   '(default ((t (:background "#F5F5F5" :foreground "black"))))
   '(font-lock-string-face ((t (:foreground "#008800"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#000088"))))
   '(font-lock-variable-name-face ((t (:foreground "#880000"))))
   '(font-lock-constant-face ((t (:foreground "#000088"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#880000"))))
   '(font-lock-type-face ((t (:bold t :foreground "#880000"))))
   '(font-lock-comment-face ((t (:italic t :foreground "#666666"))))
   '(font-lock-builtin-face ((t (:foreground "#6B9FA7"))))
   '(mmm-code-submode-face ((t (:background "#dedede"))))
   '(mmm-output-submode-face ((t (:background "#dedede"))))
   '(region ((t (:background "#999999"))))
   '(show-paren-match ((t (:background "Red3"))))
   '(cursor ((t (:background "Red2"))))
   '(ido-first-match ((t (:bold t :foreground "#880000"))))))

(defun color-theme-my-darc ()
  (interactive)
  (color-theme-arjen)
  (custom-set-faces
   '(default ((t (:background "#050505" :bold t))))
   '(darcsum-need-action-marked-face ((t (:foreground "Red"))))
   '(mmm-declaration-submode-face ((t (:background "#222266"))))
   '(mmm-default-submode-face ((t (:background "#202020"))))
   '(mmm-output-submode-face  ((t (:background "#303030"))))
   '(mmm-code-submode-face    ((t (:background "#202020"))))
   '(mmm-comment-submode-face ((t (:background "gray55"))))

   '(nxml-element-colon-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-prefix-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-local-name-face    ((t (:bold t :foreground "#00cc00"))))

   '(nxml-tag-delimiter-face    ((t (:bold t :foreground "#00cc00"))))
   '(nxml-tag-slash-face    ((t (:bold t :foreground "#00cc00"))))

   '(font-lock-comment-face ((t (:italic t :foreground "grey55"))))
   '(font-lock-string-face ((t (:foreground "#aaff55"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#0861bf"))))
   '(font-lock-warning-face ((t (:underline t :foreground "VioletRed"))))
   '(font-lock-constant-face ((t (:foreground "#92D229"))))
   '(font-lock-type-face ((t (:foreground "#8888ff"))))
   '(font-lock-variable-name-face ((t (:foreground "Red"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#00cc00"))))
   '(font-lock-builtin-face ((t (:foreground "Red3"))))))

(defun color-theme-textmate()
  (interactive)
  (color-theme-arjen)
  (custom-set-faces
   '(default ((t (:background "#323232"))))
;   '(region ((t :background "#4A6152")))
   '(match ((t :background "#545C72")))
   '(region ((t :background "#4A6152")))
   '(font-lock-comment-face ((t (:italic t :foreground "#A541D6"))))
   '(font-lock-string-face ((t (:foreground "#A5C255"))))
   '(font-lock-keyword-face ((t (:bold t :foreground "#FF6D08"))))
   '(font-lock-warning-face ((t (:underline t :foreground "VioletRed"))))
   '(font-lock-constant-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-type-face ((t (:bold t :foreground "#6666FF"))))
   '(font-lock-variable-name-face ((t (:foreground "#FF6666"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "#FFDB08"))))
   '(font-lock-builtin-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-preprocessor-face ((t (:background "#313031"))))

   '(compilation-info ((t (:inherit 'font-lock-string-face))))
   '(compilation-error ((t (:inherit 'font-lock-variable-name-face))))
   '(compilation-line-number ((t (:inherit 'font-lock-builtin-face))))

   '(flymake-errline ((t :underline "red")))
   '(flymake-warnline ((t :underline "green")))

   '(mmm-declaration-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-default-submode-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-output-submode-face  ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-code-submode-face    ((t (:inherit 'font-lock-preprocessor-face))))
   '(mmm-comment-submode-face ((t (:inherit 'font-lock-comment-face))))

   '(nxml-element-colon-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-prefix-face    ((t (:bold t :foreground "#92D229"))))
   '(nxml-element-local-name-face    ((t (:bold t :foreground "#00cc00"))))
   '(nxml-tag-delimiter-face    ((t (:bold t :foreground "#00cc00"))))
   '(nxml-tag-slash-face    ((t (:bold t :foreground "#00cc00"))))

   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))

   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   '(mode-line ((t (:family "-outline-Arial-normal-r-normal-normal-*-*-96-96-p-*-iso10646-1"
                    :background "#EFEFEF"
                    :foreground "#333133"
                    :box (:line-width 1 :color "#555555")))))
   '(mode-line-inactive ((t (:inherit 'mode-line :background "#AEAEAE"))))
   '(mode-line-buffer-id ((t (:inherit 'mode-line :bold t))))
   '(mode-line-highlight ((t (:inherit 'mode-line-inactive))))
;   '(modeline-mousable ((t (:inherit 'mode-line))))
;   '(modeline-mousable-minor-mode ((t (:inherit 'mode-line))))

   '(completion-dynamic-face ((t (:inherit 'font-lock-preprocessor-face))))
   '(highlight ((t (:inherit 'font-lock-preprocessor-face))))))

;; based on the idleFingers Textmate theme
(defun color-theme-textmate2()
  (interactive)
  (color-theme-arjen)
  (custom-set-faces
   '(default ((t (:background "#2B2B2B"))))
   '(match ((t :background "#4A6152")))
   '(cursor ((t (:background "#FFFB7B"))))
   '(region ((t :background "#505C74")))
   '(font-lock-comment-face ((t (:italic t :foreground "#B150E7"))))
   '(font-lock-string-face ((t (:foreground "#A5F26E"))))
   '(font-lock-keyword-face ((t (:foreground "#DC762A")))) ;; #CC7832"
   '(font-lock-warning-face ((t (:underline "red"))))
   '(font-lock-constant-face ((t (:foreground "#6BCFF7"))))
   '(font-lock-type-face ((t (:foreground "#CACE28")))) ;; "#8888ff"
   '(font-lock-variable-name-face ((t (:foreground "#D0D0F3")))) ;;
   '(font-lock-function-name-face ((t (:foreground "#FFFB7B")))) ;; "#E8BF6A"
   '(font-lock-builtin-face ((t (:foreground "#59ACC2"))))
   '(font-lock-preprocessor-face ((t (:background "#222222" :foreground "#CDCDCD"))))

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

   '(ido-first-match ((t (:inherit 'font-lock-string-face))))
   '(ido-subdir ((t (:inherit 'font-lock-function-name-face))))

   '(minibuffer-noticeable-prompt ((t (:inherit 'font-lock-builtin-face :bold t))))

   '(mode-line ((t (:background "#EFEFEF"
                    :foreground "#333133"
                    :height 0.9
                    :box (:line-width 2 :color "#EEEEEE" :style nil)))))
   '(mode-line-inactive ((t (:inherit 'mode-line :background "#AEAEAE"))))
   '(mode-line-buffer-id ((t (:inherit 'mode-line :bold t))))
   '(mode-line-highlight ((t (:inherit 'mode-line-inactive))))

   '(tooltip ((t
               (:family
                "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso10646-1"
                :height 0.8
                :foreground "systeminfotext"
                :background "systeminfowindow"
                :inherit (variable-pitch)))))

   '(completion-dynamic-face ((t (:inherit 'match))))
   '(highlight ((t (:inherit 'match))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load color-theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
(color-theme-textmate2)
