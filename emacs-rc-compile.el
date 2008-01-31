(require 'custom)

(defvar all-overlays ())

(setq compilation-window-height 10)

(defun delete-this-overlay(overlay is-after begin end &optional len)
  (delete-overlay overlay)
  )

(defun highlight-current-line()
  (interactive)
  (let (beg end current-point error-line-overlay)
    (setq current-point (point))
    (beginning-of-line)
    (setq beg (point))
    (forward-line 1)
    (setq end (point))
    ;; Create and place the overlay
    (setq error-line-overlay (make-overlay 1 1))

    ;; Append to list of all overlays
    (setq all-overlays (cons error-line-overlay all-overlays))

    (overlay-put error-line-overlay
                 'face '(background-color . "pink"))
    (overlay-put error-line-overlay
                 'modification-hooks (list 'delete-this-overlay))
    (move-overlay error-line-overlay beg end)
    (goto-char current-point)
    )
  )

(defun delete-all-overlays()
  (while all-overlays
    (delete-overlay (car all-overlays))
    (setq all-overlays (cdr all-overlays))
    )
  )

(defun highlight-error-lines(compilation-buffer, process-result)
  (interactive)
  (delete-all-overlays)
  (condition-case nil
      (while t
        (my-next-error)
        (highlight-current-line)
        )
    (error nil))
)

(setq compilation-finish-functions 'highlight-error-lines)

(defun compile-sln()
  (interactive)
  (let (vcpath sln file com soltn)
    (setq vcpath "\"c:/Program Files/Microsoft Visual Studio .NET 2003/Common7/IDE/devenv.com\"")
    (setq com nil)
    (setq soltn "../vc/*.sln")
    (setq file (file-expand-wildcards soltn t))
    (message (car file))
    (if (car file)
        (setq com (concat vcpath " " (car file) " /build Debug")))

    (if (not com)
        (progn
          (setq file (file-expand-wildcards "Makefile" t))
          (if (car file)
              (setq com (concat "make " (car file))))))
    (if com
        (compile com))
))

;; (defun my-recompile ()
;;     "Run compile and resize the compile window closing the old one if necessary"
;;     (interactive)
;;     (progn
;;       (if (get-buffer "*compilation*") ; If old compile window exists
;;          (progn
;;            (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
;;            (kill-buffer "*compilation*") ; and kill the buffers
;;            )
;;         )
;;       (call-interactively 'compile-sln)
;;       (enlarge-window 20)
;;       ))

(defun my-next-error ()
    "Move point to next error and highlight it"
    (interactive)
    (progn
      (next-error)
      (end-of-line-nomark)
      (beginning-of-line-mark)
      ))

(defun my-previous-error ()
  "Move point to previous error and highlight it"
  (interactive)
  (progn
    (previous-error)
    (end-of-line-nomark)
    (beginning-of-line-mark)
    ))
