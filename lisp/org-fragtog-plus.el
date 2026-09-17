;;; org-fragtog-plus.el --- Automatic Org formula previews -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2") (org "9.7.11"))
;; Keywords: outlines, tex

;;; Commentary:
;; Toggle formula source at point using a buffer-local preview backend.
;; The default backend uses Org's LaTeX preview commands.

;;; Code:
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'seq)

(defgroup org-fragtog-plus nil "Automatic Org formula previews." :group 'org)

(defcustom org-fragtog-plus-delay 0
  "Idle seconds before revealing the formula at point."
  :type 'number :group 'org-fragtog-plus)

(defcustom org-fragtog-plus-backend #'org-fragtog-plus-latex-backend
  "Function called with ACTION and optional FRAGMENT.
ACTION is `fragment', `show', `hide', or `clear'.
`fragment' returns the Org element at point, or nil.  Its source extent
is :begin plus the length of :value, excluding trailing whitespace.
`show' and `hide' receive that element and must be idempotent.
`clear' removes previews and invalidates pending work in this buffer.
Asynchronous backends should leave the current fragment hidden when
this mode is active.  Use `org-fragtog-plus-set-backend' to switch an
active buffer's backend."
  :type 'function :group 'org-fragtog-plus)
(make-variable-buffer-local 'org-fragtog-plus-backend)

(defvar-local org-fragtog-plus--start nil)
(defvar-local org-fragtog-plus--timer nil)
(defvar-local org-fragtog-plus--tick nil)

(defun org-fragtog-plus-latex-backend (action &optional fragment)
  "Handle ACTION on FRAGMENT using Org's built-in LaTeX preview."
  (pcase action
    ('fragment
     (let ((element (org-element-context)))
       (when (and (memq (org-element-type element) '(latex-fragment latex-environment))
                  (< (point) (+ (org-element-property :begin element)
                                (length (org-element-property :value element)))))
         element)))
    ('show
     (save-excursion
       (goto-char (org-element-property :begin fragment))
       (unless (seq-some (lambda (overlay) (eq (overlay-get overlay 'org-overlay-type) 'org-latex-overlay))
                         (overlays-at (point)))
         (let ((mark-active nil)) (org-latex-preview)))))
    ('hide
     (org-clear-latex-preview (org-element-property :begin fragment)
                              (+ (org-element-property :begin fragment)
                                 (length (org-element-property :value fragment)))))
    ('clear (org-clear-latex-preview))))

(defun org-fragtog-plus--cancel-timer ()
  "Cancel a pending reveal."
  (when org-fragtog-plus--timer
    (cancel-timer org-fragtog-plus--timer)
    (setq org-fragtog-plus--timer nil)))

(defun org-fragtog-plus--reveal (buffer)
  "Reveal the current fragment in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-fragtog-plus--timer nil)
      (when-let* ((fragment (funcall org-fragtog-plus-backend 'fragment)))
        (funcall org-fragtog-plus-backend 'hide fragment)))))

(defun org-fragtog-plus--post-command ()
  "Preview the fragment left behind and reveal the one at point."
  (let* ((fragment (funcall org-fragtog-plus-backend 'fragment))
         (start (and fragment (org-element-property :begin fragment)))
         (old (and org-fragtog-plus--start (marker-position org-fragtog-plus--start)))
         (tick (buffer-chars-modified-tick)))
    (when (or (not (equal start old)) (not (equal tick org-fragtog-plus--tick)))
      (org-fragtog-plus--cancel-timer)
      (when (and old (not (equal start old)))
        (save-excursion
          (goto-char old)
          (when-let* ((previous (funcall org-fragtog-plus-backend 'fragment)))
            (funcall org-fragtog-plus-backend 'show previous))))
      (when org-fragtog-plus--start (set-marker org-fragtog-plus--start nil))
      (setq org-fragtog-plus--start (and start (copy-marker start))
            org-fragtog-plus--tick tick)
      (when fragment
        (if (> org-fragtog-plus-delay 0)
            (setq org-fragtog-plus--timer
                  (run-with-idle-timer org-fragtog-plus-delay nil
                                       #'org-fragtog-plus--reveal (current-buffer)))
          (org-fragtog-plus--reveal (current-buffer)))))))

(defun org-fragtog-plus--clear ()
  "Release this buffer's tracking state and previews."
  (org-fragtog-plus--cancel-timer)
  (when org-fragtog-plus--start (set-marker org-fragtog-plus--start nil))
  (setq org-fragtog-plus--start nil org-fragtog-plus--tick nil)
  (funcall org-fragtog-plus-backend 'clear))

(defun org-fragtog-plus-set-backend (backend)
  "Select BACKEND for this buffer, clearing the previous backend's state."
  (unless (eq backend org-fragtog-plus-backend)
    (when (bound-and-true-p org-fragtog-plus-mode) (org-fragtog-plus--clear))
    (setq-local org-fragtog-plus-backend backend)
    (when (bound-and-true-p org-fragtog-plus-mode) (org-fragtog-plus--post-command))))

;;;###autoload
(define-minor-mode org-fragtog-plus-mode
  "Reveal Org formulas at point and preview them when point leaves.
Use this instead of org-fragtog-mode in a given buffer."
  :lighter " Frag+"
  (if org-fragtog-plus-mode
      (progn
        (unless (derived-mode-p 'org-mode)
          (setq org-fragtog-plus-mode nil)
          (user-error "Org Fragtog Plus requires Org"))
        (add-hook 'post-command-hook #'org-fragtog-plus--post-command nil t)
        (add-hook 'kill-buffer-hook #'org-fragtog-plus--clear nil t)
        (add-hook 'change-major-mode-hook #'org-fragtog-plus--clear nil t)
        (org-fragtog-plus--post-command))
    (remove-hook 'post-command-hook #'org-fragtog-plus--post-command t)
    (remove-hook 'kill-buffer-hook #'org-fragtog-plus--clear t)
    (remove-hook 'change-major-mode-hook #'org-fragtog-plus--clear t)
    (org-fragtog-plus--clear)))

(provide 'org-fragtog-plus)
;;; org-fragtog-plus.el ends here
