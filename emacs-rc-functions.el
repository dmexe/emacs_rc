;; -*- coding: utf-8-unix; -*-

;;; ---------------------------------------------------------
;;; - unix <> dos
;;;
(defun my/dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun my/unix2dos ()
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

(defun my/javadoc-return ()
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


;;; ---------------------------------------------------------
;;; - From Steve Yegge
;;;
(defun my/rename-file-and-buffer (new-name)
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

(defun my/move-buffer-file (dir)
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