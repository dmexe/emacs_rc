;; -*- coding: utf-8-unix; -*-

;;; ---------------------------------------------------------
;;; - Toggle input method for windows
;;;
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

  ;; replace M-TAB to C-TAB
  (define-key function-key-map [(control tab)] [?\M-\t]))


;;; ---------------------------------------------------------
;;; - Carbon
;;;
(when (eq system-type 'darwin)
  ;; skip to pass keys to system
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq mac-pass-command-to-system nil))


;;; ---------------------------------------------------------
;;; - Keys
;;;
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

(global-set-key (kbd "<C-f1>") 'emacs-ru-keyhelp)
(global-set-key (kbd "<f8>") 'mmm-parse-buffer)
(global-set-key (kbd "\C-x <tab>") 'cua-set-rectangle-mark)
(global-set-key (kbd "M-s") 'anything)

(case system-type
  ('darwin
   (global-set-key (kbd "M-SPC") 'toggle-input-method))
  ('windows-nt
   (global-set-key (kbd "<language-change>") 'safe-language-change)))
