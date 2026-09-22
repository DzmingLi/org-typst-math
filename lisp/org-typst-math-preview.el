;;; org-typst-math-preview.el --- Inline Typst previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Part of org-typst-math.  Reuses the export helper for asynchronous SVG batches.

;;; Code:
(require 'org-typst-math)
(require 'color)
(require 'image)

(defun org-typst-math-preview--scale ()
  "Scale the helper's 12pt output to Emacs's default face size."
  (/ (face-attribute 'default :height nil 'default) 120.0))

(defcustom org-typst-math-preview-inline-padding 6
  "Extra vertical space in pixels for inline formula images."
  :type 'natnum :group 'org-typst-math)

(defvar-local org-typst-math-preview--overlays nil)
(defvar-local org-typst-math-preview--diagnostics nil)
(defvar-local org-typst-math-preview--process-error nil)
(defvar-local org-typst-math-preview--generation 0)
(defvar-local org-typst-math-preview--pending nil)
(defvar-local org-typst-math-preview--dirty nil)
(defvar-local org-typst-math-preview--timer nil)
(defvar-local org-typst-math-preview--scheduled nil)
(defvar org-fragtog-plus-backend)
(declare-function org-fragtog-plus-latex-backend "org-fragtog-plus" (action &optional fragment))
(declare-function org-fragtog-plus-set-backend "org-fragtog-plus" (backend))

(defun org-typst-math-preview-clear ()
  "Remove this buffer's previews and invalidate outstanding results."
  (interactive)
  (cl-incf org-typst-math-preview--generation)
  (when org-typst-math-preview--timer
    (cancel-timer org-typst-math-preview--timer))
  (setq org-typst-math-preview--timer nil
        org-typst-math-preview--scheduled nil)
  (setq org-typst-math-preview--dirty nil)
  (mapc #'delete-overlay org-typst-math-preview--overlays)
  (setq org-typst-math-preview--overlays nil)
  (mapc #'delete-overlay org-typst-math-preview--diagnostics)
  (setq org-typst-math-preview--diagnostics nil
        org-typst-math-preview--process-error nil)
  (org-typst-math-preview--publish-diagnostics))

(defun org-typst-math-preview--show (overlay visible)
  "Show OVERLAY's image and alignment when VISIBLE, otherwise its source."
  (overlay-put overlay 'display (and visible (overlay-get overlay 'org-typst-image)))
  (overlay-put overlay 'before-string (and visible (overlay-get overlay 'org-typst-before)))
  (overlay-put overlay 'after-string (and visible (overlay-get overlay 'org-typst-after))))

(defun org-typst-math-preview--toggle ()
  "Reveal the formula at point and display the other previews."
  (dolist (overlay org-typst-math-preview--overlays)
    (when (overlay-buffer overlay)
      (org-typst-math-preview--show
       overlay (not (and org-typst-math-mode
                         (bound-and-true-p org-fragtog-plus-mode)
                         (eq org-fragtog-plus-backend #'org-typst-math-preview-backend)
                         (<= (overlay-start overlay) (point))
                         (< (point) (overlay-end overlay))))))))

(defun org-typst-math-preview--modified (overlay after &rest _)
  "Remove OVERLAY after its underlying source changes."
  (when after (delete-overlay overlay)))

(defun org-typst-math-preview--publish-diagnostics ()
  "Publish this buffer's current formula and process diagnostics quietly."
  (let ((preamble (org-typst-math-preview--preamble)) entries kept)
    (dolist (overlay org-typst-math-preview--diagnostics)
      (when (overlay-buffer overlay)
        (let* ((element (save-excursion
                          (goto-char (overlay-start overlay)) (org-element-context)))
               (fragment (and (org-element-type-p element 'latex-fragment)
                              (org-typst-math-fragment element))))
          (if (and fragment
                   (= (org-element-property :begin element) (overlay-start overlay))
                   (equal preamble (overlay-get overlay 'preamble)))
              (progn
                (push overlay kept)
                (seq-doseq (diagnostic (overlay-get overlay 'diagnostics))
                  (push (org-typst-math--diagnostic-entry
                         diagnostic element fragment preamble) entries)))
            (delete-overlay overlay)))))
    (setq org-typst-math-preview--diagnostics (nreverse kept))
    (when org-typst-math-preview--process-error
      (push (org-typst-math--process-error-entry org-typst-math-preview--process-error) entries))
    (let ((display-buffer-alist '((".*" (display-buffer-no-window) (allow-no-window . t)))))
      (org-typst-math--diagnostics (nreverse entries)))))

(defun org-typst-math-preview--save-diagnostics (element diagnostics preamble)
  "Replace ELEMENT's DIAGNOSTICS, keeping other formulas' results and PREAMBLE."
  (let ((start (org-element-property :begin element)))
    (dolist (overlay org-typst-math-preview--diagnostics)
      (when (and (overlay-buffer overlay) (= (overlay-start overlay) start))
        (delete-overlay overlay)))
    (when (and diagnostics (> (seq-length diagnostics) 0))
      (let ((overlay (make-overlay start (+ start (length (org-element-property :value element)))
                                   nil t nil)))
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'modification-hooks '(org-typst-math-preview--modified))
        (overlay-put overlay 'diagnostics diagnostics)
        (overlay-put overlay 'preamble preamble)
        (push overlay org-typst-math-preview--diagnostics)))))

(defun org-typst-math-preview--apply (result elements fragments preamble &optional preserve)
  "Install RESULT for ELEMENTS and FRAGMENTS, mapping PREAMBLE diagnostics.
When PRESERVE is non-nil, keep previews outside ELEMENTS."
  (setq org-typst-math-preview--overlays
        (seq-filter #'overlay-buffer org-typst-math-preview--overlays))
  (unless preserve
    (mapc #'delete-overlay org-typst-math-preview--diagnostics)
    (setq org-typst-math-preview--diagnostics nil))
  (setq org-typst-math-preview--process-error nil)
  (let (kept)
    (cl-mapc
     (lambda (element fragment item)
       (unless (plist-get (plist-get item :artifact) :svg)
         (when-let* ((old (org-typst-math-preview--fragment-overlay element)))
           (delete-overlay old)))
       (when-let* ((svg (plist-get (plist-get item :artifact) :svg)))
         (let* ((start (org-element-property :begin element))
                (end (+ start (length (org-element-property :value element))))
                ;; Boundary insertions belong outside the formula.  Including
                ;; them hides newly typed text behind the image without firing
                ;; `modification-hooks' until a later deletion.
                (overlay (or (org-typst-math-preview--fragment-overlay element)
                             (make-overlay start end nil t nil)))
                (scale (org-typst-math-preview--scale))
                (block (eq (plist-get fragment :display) t))
                (margin (cons 0 (if block 0 (ceiling (/ org-typst-math-preview-inline-padding 2.0)))))
                (key (list (secure-hash 'sha256 svg) scale block margin)))
           (unless (equal key (overlay-get overlay 'org-typst-image-key))
             (overlay-put overlay 'org-typst-image
                          (create-image svg 'svg t :ascent 'center
                                        :scale scale :margin margin))
             (overlay-put overlay 'org-typst-image-key key))
           (move-overlay overlay start end)
           (overlay-put overlay 'org-typst-before nil)
           (overlay-put overlay 'org-typst-after nil)
           (when (eq (plist-get fragment :display) t)
             (let ((half-width (/ (car (image-size (overlay-get overlay 'org-typst-image) t)) 2.0)))
               ;; Pixel offsets retain the correct alignment when the window resizes.
               (overlay-put overlay 'org-typst-before
                            (concat (save-excursion (goto-char start) (unless (bolp) "\n"))
                                    (propertize " " 'display
                                                `(space :align-to (- center (,half-width))))))
               (overlay-put overlay 'org-typst-after
                            (save-excursion (goto-char end) (unless (eolp) "\n")))))
           (overlay-put overlay 'evaporate t)
           (overlay-put overlay 'modification-hooks '(org-typst-math-preview--modified))
           (push overlay kept)
           (cl-pushnew overlay org-typst-math-preview--overlays)))
       (org-typst-math-preview--save-diagnostics element (plist-get item :diagnostics) preamble))
     elements fragments (append (plist-get result :items) nil))
    (unless preserve
      (dolist (overlay org-typst-math-preview--overlays)
        (unless (memq overlay kept) (delete-overlay overlay))))
    (setq org-typst-math-preview--overlays
          (seq-filter #'overlay-buffer org-typst-math-preview--overlays))
    (org-typst-math-preview--publish-diagnostics))
  (org-typst-math-preview--toggle))

(defun org-typst-math-preview-diagnostics ()
  "Show the shared Typst diagnostics buffer."
  (interactive)
  (when (or org-typst-math-preview--diagnostics org-typst-math-preview--process-error)
    (org-typst-math-preview--publish-diagnostics))
  (display-buffer (get-buffer-create "*Org Typst diagnostics*")))

(defun org-typst-math-preview--preamble ()
  "Return the current buffer's math preamble."
  (mapconcat #'identity
             (cdr (assoc "TYPST_HEADER" (org-collect-keywords '("TYPST_HEADER")))) "\n"))

(defun org-typst-math-preview--request-refresh (&optional missing-only)
  "Coalesce refresh requests until the next timer turn.
MISSING-ONLY requests cannot downgrade a queued full refresh."
  (add-hook 'kill-buffer-hook #'org-typst-math-preview-clear nil t)
  (add-hook 'change-major-mode-hook #'org-typst-math-preview-clear nil t)
  (if org-typst-math-preview--pending
      (org-typst-math-preview-refresh missing-only)
    (setq org-typst-math-preview--scheduled
          (if (or (not missing-only) (eq org-typst-math-preview--scheduled 'all))
              'all 'missing))
    (unless org-typst-math-preview--timer
      (let ((buffer (current-buffer))
            (generation org-typst-math-preview--generation))
        (setq org-typst-math-preview--timer
              (run-at-time
               0 nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (= generation org-typst-math-preview--generation)
                       (org-typst-math-preview-refresh
                        (eq org-typst-math-preview--scheduled 'missing))))))))))))

(defun org-typst-math-preview-refresh (&optional missing-only)
  "Asynchronously preview math using the shared helper.
With MISSING-ONLY, retain existing previews and render only missing ones."
  (interactive)
  (when (eq org-typst-math-preview--scheduled 'all)
    (setq missing-only nil))
  (when org-typst-math-preview--timer
    (cancel-timer org-typst-math-preview--timer))
  (setq org-typst-math-preview--timer nil
        org-typst-math-preview--scheduled nil)
  (unless (image-type-available-p 'svg) (user-error "Emacs needs SVG image support"))
  (add-hook 'kill-buffer-hook #'org-typst-math-preview-clear nil t)
  (add-hook 'change-major-mode-hook #'org-typst-math-preview-clear nil t)
  (setq org-typst-math-preview--dirty
        (if (or (not missing-only) (eq org-typst-math-preview--dirty 'all))
            'all 'missing))
  (unless org-typst-math-preview--pending
    (setq missing-only (eq org-typst-math-preview--dirty 'missing)
          org-typst-math-preview--dirty nil)
    (let* ((buffer (current-buffer))
           (generation (cl-incf org-typst-math-preview--generation))
           (preamble (org-typst-math-preview--preamble))
           (rgb (color-values (face-foreground 'default nil t)))
           (foreground (if rgb (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)) "#000000"))
           elements fragments items trackers)
      (org-element-map (org-element-parse-buffer) 'latex-fragment
                       (lambda (element)
                         (when-let* ((fragment (org-typst-math-fragment element)))
                           (unless (and missing-only (org-typst-math-preview--fragment-overlay element))
                             (push element elements)
                             (push fragment fragments)
                             ;; Track each source independently: typing elsewhere must not
                             ;; discard a finished render, and insertions before it move it.
                             (let* ((start (org-element-property :begin element))
                                    (tracker (make-overlay start
                                                           (+ start (length (org-element-property :value element)))
                                                           nil t nil)))
                               (overlay-put tracker 'evaporate t)
                               (overlay-put tracker 'modification-hooks '(org-typst-math-preview--modified))
                               (push tracker trackers))
                             (push (list :id (number-to-string (length elements))
                                         :source (plist-get fragment :source)
                                         :display (plist-get fragment :display)
                                         :foreground foreground) items)))))
      (setq elements (nreverse elements) fragments (nreverse fragments)
            trackers (nreverse trackers))
      (if (null items) (unless missing-only (org-typst-math-preview-clear))
        (setq org-typst-math-preview--pending t)
        (cl-labels
            ((finish ()
               (mapc #'delete-overlay trackers)
               (setq org-typst-math-preview--pending nil)
               (when org-typst-math-preview--dirty
                 (org-typst-math-preview-refresh
                  (eq org-typst-math-preview--dirty 'missing)))))
          (condition-case err
              (typst-client-convert-async
               default-directory preamble (nreverse items)
               (lambda (result)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (unwind-protect
                         (when (= generation org-typst-math-preview--generation)
                           (if (not (equal preamble (org-typst-math-preview--preamble)))
                               (setq org-typst-math-preview--dirty 'all)
                             (let (valid-elements valid-fragments valid-items)
                               (cl-mapc
                                (lambda (tracker element fragment item)
                                  (when (and (overlay-buffer tracker)
                                             (equal (org-element-property :value element)
                                                    (buffer-substring-no-properties
                                                     (overlay-start tracker) (overlay-end tracker))))
                                    ;; Surrounding edits can turn unchanged text
                                    ;; into a code block or break its delimiters.
                                    (let ((current (save-excursion
                                                     (goto-char (overlay-start tracker))
                                                     (org-element-context))))
                                      (when (and (org-element-type-p current 'latex-fragment)
                                                 (= (org-element-property :begin current)
                                                    (overlay-start tracker))
                                                 (equal (org-element-property :value current)
                                                        (org-element-property :value element)))
                                        (push current valid-elements)
                                        (push fragment valid-fragments)
                                        (push item valid-items)))))
                                trackers elements fragments (append (plist-get result :items) nil))
                               (when valid-items
                                 (org-typst-math-preview--apply
                                  (list :items (vconcat (nreverse valid-items)))
                                  (nreverse valid-elements) (nreverse valid-fragments) preamble t)))))
                       (finish)))))
               (lambda (error)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (= generation org-typst-math-preview--generation)
                       (setq org-typst-math-preview--process-error (plist-get error :message))
                       (org-typst-math-preview--publish-diagnostics)
                       (message "Typst preview: %s" org-typst-math-preview--process-error))
                     (finish))))
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
         (org-typst-math-preview--show overlay t)
       (org-typst-math-preview--request-refresh t)))
    ('hide
     (when-let* ((overlay (org-typst-math-preview--fragment-overlay fragment)))
       (org-typst-math-preview--show overlay nil)))
    ('clear (org-typst-math-preview-clear))))

(defun org-typst-math-preview--setup-backend ()
  "Select the Typst backend according to this buffer's math mode."
  (if org-typst-math-mode
      (org-fragtog-plus-set-backend #'org-typst-math-preview-backend)
    (when (eq org-fragtog-plus-backend #'org-typst-math-preview-backend)
      (org-typst-math-preview-clear)
      (org-fragtog-plus-set-backend #'org-fragtog-plus-latex-backend))))

(defun org-typst-math-preview-link (overlay source)
  "Render Typst SOURCE in an Org-owned link OVERLAY asynchronously.
SOURCE is markup text or a zero-argument function returning it.  This is a
generic entry point for link adapters, independent of math-mode and Quiver.
Read TYPST_HEADER and resolve imports in the current document directory.
Return non-nil while rendering.  Editing or clearing the Org overlay,
changing headers, or starting a new request invalidates late results.
Conversion and rendering failures leave the link readable with help text."
  (when (display-graphic-p)
    (let* ((buffer (current-buffer))
           (root default-directory)
           (preamble (org-typst-math-preview--preamble))
           (text (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
           (token (gensym "typst-link-preview"))
           (foreground (color-values (face-foreground 'default nil t)))
           (colour (if foreground
                       (apply #'format "#%02x%02x%02x" (mapcar (lambda (c) (/ c 257)) foreground))
                     "#000000")))
      (overlay-put overlay 'org-typst-link-request token)
      (cl-labels
          ((current-p ()
             (and (buffer-live-p buffer) (eq (overlay-buffer overlay) buffer)
                  (eq (overlay-get overlay 'org-typst-link-request) token)
                  (with-current-buffer buffer
                    (and (equal preamble (org-typst-math-preview--preamble))
                         (equal text (buffer-substring-no-properties
                                      (overlay-start overlay) (overlay-end overlay)))))))
           (fail (message)
             (when (current-p)
               (overlay-put overlay 'display nil)
               (overlay-put overlay 'before-string nil)
               (overlay-put overlay 'help-echo (concat "Typst preview: " message))
               (message "Typst preview: %s" message)))
           (render (source)
             (when (current-p)
               (with-current-buffer buffer
                 (typst-client-render-async
                  root preamble
                  (list (list :id "content" :source source :foreground colour :display t))
                  (lambda (result)
                    (when (current-p)
                      (with-current-buffer buffer
                        (let* ((item (elt (plist-get result :items) 0))
                               (svg (plist-get (plist-get item :artifact) :svg)))
                          (if (not svg)
                              (fail (mapconcat (lambda (d) (plist-get d :message))
                                               (plist-get item :diagnostics) "; "))
                            (condition-case err
                                (let ((image (create-image svg 'svg t :ascent 'center
                                                           :scale (org-typst-math-preview--scale)
                                                           :max-width (floor (* 0.9 (window-body-width nil t))))))
                                  (overlay-put overlay 'display image)
                                  (overlay-put overlay 'face 'default)
                                  (overlay-put overlay 'help-echo "Typst preview; C-c C-o follows the link")
                                  (overlay-put overlay 'before-string
                                               (propertize " " 'display
                                                           `(space :align-to (- center (0.5 . ,image))))))
                              (error (fail (error-message-string err)))))))))
                  (lambda (err) (fail (or (plist-get err :message) (format "%S" err)))))))))
        (condition-case err
            (render (if (functionp source) (funcall source) source))
          (error (fail (error-message-string err)))))
      t)))

(provide 'org-typst-math-preview)
;;; org-typst-math-preview.el ends here
