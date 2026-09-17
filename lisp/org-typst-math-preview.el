;;; org-typst-math-preview.el --- Inline Typst previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Part of org-typst-math.  Reuses the export helper for asynchronous SVG batches.

;;; Code:
(require 'org-typst-math)
(require 'color)

(defcustom org-typst-math-preview-scale 1.2
  "Scale of SVG preview images."
  :type 'number :group 'org-typst-math)

(defvar-local org-typst-math-preview--overlays nil)
(defvar-local org-typst-math-preview--generation 0)
(defvar-local org-typst-math-preview--pending nil)
(defvar-local org-typst-math-preview--dirty nil)
(defvar org-fragtog-plus-backend)
(declare-function org-fragtog-plus-latex-backend "org-fragtog-plus" (action &optional fragment))
(declare-function org-fragtog-plus-set-backend "org-fragtog-plus" (backend))

(defun org-typst-math-preview-clear ()
  "Remove this buffer's previews and invalidate outstanding results."
  (interactive)
  (cl-incf org-typst-math-preview--generation)
  (setq org-typst-math-preview--dirty nil)
  (mapc #'delete-overlay org-typst-math-preview--overlays)
  (setq org-typst-math-preview--overlays nil))

(defun org-typst-math-preview--toggle ()
  "Reveal the formula at point and display the other previews."
  (dolist (overlay org-typst-math-preview--overlays)
    (when (overlay-buffer overlay)
      (overlay-put overlay 'display
                   (unless (and org-typst-math-mode
                                (bound-and-true-p org-fragtog-plus-mode)
                                (eq org-fragtog-plus-backend #'org-typst-math-preview-backend)
                                (<= (overlay-start overlay) (point))
                                (< (point) (overlay-end overlay)))
                     (overlay-get overlay 'org-typst-image))))))

(defun org-typst-math-preview--modified (overlay after &rest _)
  "Remove OVERLAY after its underlying source changes."
  (when after (delete-overlay overlay)))

(defun org-typst-math-preview--apply (result elements fragments preamble)
  "Install RESULT for ELEMENTS and FRAGMENTS, mapping PREAMBLE diagnostics."
  (mapc #'delete-overlay org-typst-math-preview--overlays)
  (setq org-typst-math-preview--overlays nil)
  (let (entries)
    (cl-mapc
     (lambda (element fragment item)
       (when-let* ((svg (plist-get (plist-get item :artifact) :svg)))
         (let* ((start (org-element-property :begin element))
                (end (+ start (length (org-element-property :value element))))
                (overlay (make-overlay start end nil nil t)))
           (overlay-put overlay 'org-typst-image
                        (create-image svg 'svg t :ascent 'center
                                      :scale org-typst-math-preview-scale))
           (overlay-put overlay 'evaporate t)
           (overlay-put overlay 'modification-hooks '(org-typst-math-preview--modified))
           (push overlay org-typst-math-preview--overlays)))
       (seq-doseq (diagnostic (plist-get item :diagnostics))
         (push (append (org-typst-math--location diagnostic element fragment preamble)
                       (list (plist-get diagnostic :severity) (plist-get diagnostic :message)))
               entries)))
     elements fragments (append (plist-get result :items) nil))
    ;; Keep typing uninterrupted; diagnostics remain available via the command.
    (let ((display-buffer-alist '((".*" (display-buffer-no-window) (allow-no-window . t)))))
      (org-typst-math--diagnostics (nreverse entries))))
  (org-typst-math-preview--toggle))

(defun org-typst-math-preview-diagnostics ()
  "Show the shared Typst diagnostics buffer."
  (interactive)
  (display-buffer (get-buffer-create "*Org Typst diagnostics*")))

(defun org-typst-math-preview-refresh ()
  "Asynchronously preview all math in the buffer using the shared helper."
  (interactive)
  (unless (image-type-available-p 'svg) (user-error "Emacs needs SVG image support"))
  (add-hook 'kill-buffer-hook #'org-typst-math-preview-clear nil t)
  (add-hook 'change-major-mode-hook #'org-typst-math-preview-clear nil t)
  (setq org-typst-math-preview--dirty t)
  (unless org-typst-math-preview--pending
    (setq org-typst-math-preview--dirty nil)
    (let* ((buffer (current-buffer))
           (generation (cl-incf org-typst-math-preview--generation))
           (tick (buffer-chars-modified-tick))
           (preamble (mapconcat #'identity
                               (cdr (assoc "TYPST_HEADER" (org-collect-keywords '("TYPST_HEADER")))) "\n"))
           (rgb (color-values (face-foreground 'default nil t)))
           (foreground (if rgb (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)) "#000000"))
           elements fragments items)
      (org-element-map (org-element-parse-buffer) 'latex-fragment
        (lambda (element)
          (when-let* ((fragment (org-typst-math-fragment element)))
            (push element elements)
            (push fragment fragments)
            (push (list :id (number-to-string (length elements))
                        :source (plist-get fragment :source)
                        :display (plist-get fragment :display)
                        :foreground foreground) items))))
      (setq elements (nreverse elements) fragments (nreverse fragments))
      (if (null items) (org-typst-math-preview-clear)
        (setq org-typst-math-preview--pending t)
        (cl-labels
            ((finish ()
               (setq org-typst-math-preview--pending nil)
               (when org-typst-math-preview--dirty
                 (org-typst-math-preview-refresh))))
          (condition-case err
              (typst-client-convert-async
               default-directory preamble (nreverse items)
               (lambda (result)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (unwind-protect
                         (when (and (= generation org-typst-math-preview--generation)
                                    (= tick (buffer-chars-modified-tick)))
                           (org-typst-math-preview--apply result elements fragments preamble))
                       (finish)))))
               (lambda (error)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (finish)
                     (message "Typst preview: %s" (plist-get error :message)))))
               "svg")
            (error (finish) (signal (car err) (cdr err)))))))))

(defun org-typst-preview (&optional clear)
  "Preview Typst math in this buffer; with prefix CLEAR, remove previews."
  (interactive "P")
  (if clear (org-typst-math-preview-clear)
    (org-typst-math-preview-refresh)))

(defun org-typst-math-preview--fragment-overlay (fragment)
  "Return the live Typst overlay at FRAGMENT's start."
  (seq-find (lambda (overlay)
              (and (overlay-buffer overlay)
                   (= (overlay-start overlay) (org-element-property :begin fragment))))
            org-typst-math-preview--overlays))

(defun org-typst-math-preview-backend (action &optional fragment)
  "Handle org-fragtog-plus ACTION on FRAGMENT using Typst."
  (pcase action
    ('fragment
     (let ((element (org-element-context)))
       (when (and (org-element-type-p element 'latex-fragment)
                  (org-typst-math-fragment element)
                  (< (point) (+ (org-element-property :begin element)
                                (length (org-element-property :value element)))))
         element)))
    ('show
     (if-let* ((overlay (org-typst-math-preview--fragment-overlay fragment)))
         (overlay-put overlay 'display (overlay-get overlay 'org-typst-image))
       (org-typst-math-preview-refresh)))
    ('hide
     (when-let* ((overlay (org-typst-math-preview--fragment-overlay fragment)))
       (overlay-put overlay 'display nil)))
    ('clear (org-typst-math-preview-clear))))

(defun org-typst-math-preview--setup-backend ()
  "Select the Typst backend according to this buffer's math mode."
  (if org-typst-math-mode
      (org-fragtog-plus-set-backend #'org-typst-math-preview-backend)
    (when (eq org-fragtog-plus-backend #'org-typst-math-preview-backend)
      (org-typst-math-preview-clear)
      (org-fragtog-plus-set-backend #'org-fragtog-plus-latex-backend))))

(provide 'org-typst-math-preview)
;;; org-typst-math-preview.el ends here
