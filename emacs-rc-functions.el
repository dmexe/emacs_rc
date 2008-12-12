;; -*- coding: utf-8-unix; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "rails-compat.el")

(defun dos2unix ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))

(defun unix2dos ()
  (interactive)
    (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))

(defun my/remove-M ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward "
" (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "
" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

(defun chm_lookup()
  "Lookup *.chm file for specific mode"
  (interactive)
  (let (chm_file)
    (cond ((string-match "^SQL" mode-name)
           (setq chm_file "d:/home/usr/manual/MySQL.chm")))
    (cond ((string-match "^PHP" mode-name)
           (setq chm_file "d:/home/usr/manual/php_en/php_manual_en.chm")))
    (cond ((string-match "^nXML" mode-name)
           (setq chm_file "d:/home/usr/manual/xsltbook.chm")))
    (cond ((string-match "Ruby" mode-name)
           (setq chm_file "d:/home/usr/manual/ProgrammingRuby.chm")))
    (cond ((string-match "^Python" mode-name)
           (setq chm_file "d:/local/python/Doc/Python24.chm")))
    (if chm_file
        (start-process "chm lookup" nil "c:/windows/keyhh.exe" "-#klink"
                       (format "'%s'" (thing-at-point 'symbol)) chm_file )
      (message (format "[%s] mode not support" mode-name))))
  )

;; PHP helper functions
(defun php-insert-phpdoc ()
  "Inserts Phpdoc header"
  (interactive)
  (insert
   "/**\n"
   " * \n"
   " * \n"
   " * @param\n"
   " * @param\n"
   " * @return\n"
   " **/\n"))

(defun php-insert-comment ()
  "Inserts php comment"
  (interactive)
  (insert
   "/**\n"
   " * \n"
   " **/\n"))

(defun php-lint ()
  "Performs a PHP lint-check on the current file."
  (interactive)
  (shell-command (concat "d:/local/php5/php.exe -l " (buffer-file-name))))

(defun emacs-ru-keyhelp ()
  "Help for emacs keys"
  (interactive)
  (start-process "ru-refcard" nil
                 "c:/Program Files/Adobe/Acrobat 5.0/Reader/AcroRd32.exe"
                 "d:/home/usr/manual/ru-refcard.pdf"))

(defun switch-cpp-h ()
  "Switch CPP/H"
  (interactive)
  (let (buf file)
    (setq buf (buffer-file-name))
    (if (string-match "\.h$" buf 0)
        (progn
         (setq file (substring buf 0 (string-match "\.h$" buf 0)))
         (setq file (concat file ".cpp"))))
    (if (string-match "\.cpp$" buf 0)
        (progn
          (setq file (substring buf 0 (string-match "\.cpp$" buf 0)))
          (setq file (concat file ".h"))))
    (if (file-exists-p file)
        (find-file file))))


(defun my-javadoc-return ()
  "Advanced C-m for Javadoc multiline comments.
Inserts `*' at the beggining of the new line if
unless return was pressed outside the comment"
  (interactive)
  (let (last is-inside)
    (setq last (point))
    (setq is-inside
          (if (search-backward "*/" nil t)
              ;; there are some comment endings - search forward
              (if (search-forward "/*" last t)
                  't 'nil)

            ;; it's the only comment - search backward
            (goto-char last)
            (if (search-backward "/*" nil t)
                't 'nil)))
    ;; go to last char position
    (goto-char last)
    ;; the point is inside some comment, insert `*'
    (if is-inside
        (progn
          (insert "\n* ")
          (indent-for-tab-command))
      ;; else insert only new-line
      (progn
        (insert "\n")
        (indent-for-tab-command)))))

(defun my-vc-status ()
  (interactive)
  (let ((file (buffer-file-name)))
    (if (vc-registered file)
        (let ((backend (vc-backend buffer-file-name)))
          (case backend
            (GIT (when (fboundp 'git-status)
                   (git-status (file-name-directory file))))
            (SVN (when (fboundp 'svn-status)
                   (call-interactively 'svn-status)))
            (t (message "Can't found status function for %s backend" backend))))
      (message "Current file not registered in VC"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From Steve Yegge
;;

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)))))