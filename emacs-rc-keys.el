;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bind setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Windows only setup
(when (eq system-type 'windows-nt)
  (defvar safe-language-change-flag nil)
  (defun safe-language-change ()
    (interactive)
    (setq safe-language-change-flag (not safe-language-change-flag))
    (when safe-language-change-flag
      (toggle-input-method)
      (w32-toggle-lock-key 'capslock)))

  ;; skip to pass keys to windows
  (setq w32-pass-alt-to-system nil)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-pass-extra-mouse-buttons-to-system nil)
  ;; (setq w32-enable-caps-lock t)
  ;; (setq w32-enable-num-lock nil)

  ;; replace M-TAB to C-TAB
  (define-key function-key-map [(control tab)] [?\M-\t]))

;; OSX only setup
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

(global-set-key [f1] 'chm_lookup)
(global-set-key [f9] 'svn-status)
(global-set-key [f11] 'speedbar-get-focus)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key (kbd "<C-f4>") 'delete-frame)
(global-set-key (kbd "\e\el") 'goto-line)
(global-set-key (kbd "\e\ei") 'indent-region)
(global-set-key (kbd "\e\ea") 'align)
(global-set-key (kbd "\e\ev") 'my-vc-status)
(global-set-key (kbd "\e\ems") 'eshell)
(global-set-key (kbd "\e\eh") 'hs-toggle-hiding)
(global-set-key (kbd "\e\ef") 'hs-hide-all)
(global-set-key (kbd "M-%") 'query-replace-regexp)

(global-set-key (kbd "\C-c r") 'recentf-open-files)
(global-set-key (kbd "<M-right>") 'previous-multiframe-window)
(global-set-key (kbd "<M-left>") 'next-multiframe-window)
(global-set-key (kbd "<M-up>") 'windmove-up)
(global-set-key (kbd "<M-down>") 'windmove-down)

(global-set-key (kbd "<C-prior>") 'beginning-of-buffer)
(global-set-key (kbd "<C-next>") 'end-of-buffer)

(global-set-key (kbd "\C-x \C-b") 'ido-switch-buffer)
(global-set-key (kbd "\C-x b") 'ibuffer-other-window)
(global-set-key (kbd "\C-x f") 'find-file-at-point)
(global-set-key (kbd "\C-x w") 'toggle-read-only)

(global-set-key (kbd "<apps>") 'next-buffer)
(global-set-key (kbd "<lwindow>") 'prev-buffer)
(global-set-key (kbd "<language-change>") 'safe-language-change)

(global-set-key (kbd "C-'") 'hippie-expand)

(global-set-key (kbd "<C-f1>") 'emacs-ru-keyhelp)
(global-set-key (kbd "<f8>") 'mmm-parse-buffer)

(global-set-key (kbd "M-#") 'add-double-angle-quote-mark)

;(w32-toggle-lock-key 'capslock 1)
