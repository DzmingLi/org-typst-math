;;; org-typst-math-fractions.el --- Experimental text fractions -*- lexical-binding: t; -*-

;;; Commentary:
;; Opt-in prototype: lay out fractions using Emacs glyph shaping.
;; No images or helper process are involved.  Enter a fraction to edit its source.

;;; Code:
(require 'org-typst-math)
(require 'cl-lib)
(require 'composite)

(defvar-local org-typst-math-fractions--overlays nil)
(defvar-local org-typst-math-fractions--composition-table nil)
(defvar-local org-typst-math-fractions--local-table-p nil)
(defvar-local org-typst-math-fractions--installed nil)
(defvar org-typst-math-fractions--scripts t
  "Dynamically bound script setting for the current glyph layout.")

(defun org-typst-math-fractions--operand-end ()
  "Read a simple math operand, including attached scripts, at point."
  (when-let* ((end (or (org-typst-math--script-end)
                      (when (and (char-after) (> (char-after) 127)
                                 (memq (get-char-code-property (char-after) 'general-category)
                                       '(Sm So)))
                        (1+ (point))))))
    (goto-char end)
    (while (and (memq (char-after) '(?_ ?^))
                (setq end (save-excursion
                            (forward-char)
                            (skip-chars-forward " \t")
                            (org-typst-math--script-end))))
      (goto-char end))
    (point)))

(defun org-typst-math-fractions--find (beg end)
  "Find explicitly grouped fractions between BEG and END.
Return (START END NUMERATOR DENOMINATOR) entries.  Deliberately leave
chained divisions, comments, strings and unsupported operands alone."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (with-syntax-table org-typst-math--script-syntax-table
        (let (tokens fractions)
          (while (< (point) (point-max))
            (skip-chars-forward " \t\n")
            (let ((start (point)))
              (cond
               ((eobp))
               ((looking-at "//\\|/\\*")
                (forward-comment (point-max))
                (push '(barrier) tokens))
               ((eq (char-after) ?\\)
                (forward-char (min 2 (- (point-max) (point))))
                (push '(barrier) tokens))
               ((eq (char-after) ?\")
                (goto-char (or (org-typst-math--script-end) (point-max)))
                (push '(barrier) tokens))
               ((eq (char-after) ?/)
                (forward-char) (push '(slash) tokens))
               ((org-typst-math-fractions--operand-end)
                (push (list 'operand start (point)
                            (buffer-substring-no-properties start (point))) tokens))
               (t (forward-char) (push '(barrier) tokens)))))
          (setq tokens (nreverse tokens))
          (cl-loop for tail on tokens
                   for prev = nil then token
                   for token = (car tail)
                   for slash = (cadr tail)
                   for den = (caddr tail)
                   when (and (eq (car token) 'operand)
                             (eq (car slash) 'slash)
                             (eq (car den) 'operand)
                             (not (eq (car prev) 'slash))
                             (not (eq (car (cadddr tail)) 'slash))
                             (not (eq (char-before (nth 1 token)) ?#))
                             (< (- (nth 2 den) (nth 1 token)) 240)
                             (not (string-match-p "[#\"\n|]\\|\\_<frac("
                                                  (concat (nth 3 token) (nth 3 den))))
                             (not (string-match-p "//\\|/\\*\\|\\\\"
                                                  (concat (nth 3 token) (nth 3 den)))))
                   do (push (list (nth 1 token) (nth 2 den)
                                  (nth 3 token) (nth 3 den)) fractions))
          (nreverse fractions))))))

(defun org-typst-math-fractions--row (source)
  "Return display characters with script levels for SOURCE.
Each entry is (CHAR . LEVEL), where positive LEVEL means superscript."
  (when (and (string-prefix-p "(" source) (string-suffix-p ")" source))
    (with-temp-buffer
      (insert source)
      (with-syntax-table org-typst-math--script-syntax-table
        (when (eq (ignore-errors (scan-sexps 1 1)) (point-max))
          (setq source (substring source 1 -1))))))
  (with-temp-buffer
    (insert source)
    (with-syntax-table org-typst-math--script-syntax-table
      (goto-char (point-min))
      (while (and org-typst-math-fractions--scripts
                  (re-search-forward "[_^]" nil t))
        (let ((level (if (eq (char-before) ?^) 1 -1))
              (marker (1- (point))))
          (skip-chars-forward " \t")
          (when-let* ((end (org-typst-math--script-end)))
            (put-text-property marker (point) 'fraction-hidden t)
            (put-text-property (point) end 'fraction-level level))))
      (let (row)
        (dotimes (i (buffer-size))
          (unless (get-text-property (1+ i) 'fraction-hidden)
            (push (cons (char-after (1+ i))
                        (or (get-text-property (1+ i) 'fraction-level) 0)) row)))
        (nreverse row)))))

(defun org-typst-math-fractions--string (num den)
  "Encode NUM and DEN as a display string for the glyph shaper."
  ;; ASCII framing keeps all framing characters in the text font.  The rule
  ;; is buffer-local and matches only this generated representation.
  ;; Resolve buffer-local symbol choices before shaping.  Including the actual
  ;; characters in the header also separates Emacs's composition cache entries.
  (let ((replace (lambda (source)
                   (replace-regexp-in-string
                    org-typst-math--identifier-re
                    (lambda (name)
                      (let ((value (cdr (assoc name org-typst-math-entities))))
                        (cond ((characterp value) (char-to-string value))
                              ((stringp value) value)
                              (t name))))
                    source t t))))
    (concat "!otmf" (if org-pretty-entities-include-sub-superscripts "1" "0")
            "{" (funcall replace num) "|" (funcall replace den) "}!")))

(defun org-typst-math-fractions--text-box (source font)
  "Measure SOURCE in FONT, returning (WIDTH ASCENT DESCENT ITEMS).
ITEMS contain (GLYPH X Y), positioned relative to the box baseline."
  (let* ((row (org-typst-math-fractions--row source))
         (string (apply #'string (mapcar #'car row)))
         (raw (composition-get-gstring 0 (length string) font string))
         (shift (round (* (font-get font :size) 0.45)))
         (width 0) (ascent 0) (descent 0) items)
    (cl-loop for entry in row for i from 0
             for glyph = (copy-sequence (aref raw (+ i 2)))
             for y = (- (* shift (cdr entry)))
             do (when (zerop (aref glyph 3)) (throw 'unsupported nil))
             do (push (list glyph width y) items)
             do (setq width (+ width (aref glyph 4))
                      ascent (max ascent (- (aref glyph 7) y))
                      descent (max descent (+ (aref glyph 8) y))))
    (list width ascent descent (nreverse items))))

(defun org-typst-math-fractions--join (boxes)
  "Join BOXES horizontally along their baselines."
  (let ((width 0) (ascent 0) (descent 0) items)
    (dolist (box boxes)
      (dolist (item (nth 3 box))
        (push (list (car item) (+ width (nth 1 item)) (nth 2 item)) items))
      (setq width (+ width (nth 0 box))
            ascent (max ascent (nth 1 box))
            descent (max descent (nth 2 box))))
    (list width ascent descent (nreverse items))))

(defun org-typst-math-fractions--fraction-box (num den font)
  "Stack NUM and DEN boxes around a text fraction bar in FONT."
  (let* ((size (font-get font :size))
         (gap (max 2 (round (* size 0.15))))
         (axis (- (round (* size 0.25))))
         (bar (copy-sequence
               (aref (composition-get-gstring 0 1 font "─") 2)))
         (bar-width (aref bar 4))
         (width (max bar-width (+ (* gap 2) (max (car num) (car den)))))
         (ascent 0) (descent 0) items)
    (when (or (zerop (aref bar 3)) (<= bar-width 0))
      (throw 'unsupported nil))
    (cl-loop for box in (list num den) for top in '(t nil)
             for baseline = (if top (- axis gap (nth 2 box))
                              (+ axis gap (nth 1 box)))
             do (setq ascent (max ascent (- (nth 1 box) baseline))
                      descent (max descent (+ (nth 2 box) baseline)))
             do (dolist (item (nth 3 box))
                  (push (list (car item) (+ (/ (- width (car box)) 2) (nth 1 item))
                              (+ baseline (nth 2 item))) items)))
    ;; Overlapping box-drawing glyphs form one continuous fraction bar.
    (let ((x 0) (y (+ axis (/ (- (aref bar 7) (aref bar 8)) 2))))
      (while (< x width)
        (push (list (copy-sequence bar) (min x (- width bar-width)) y) items)
        (setq x (+ x bar-width))))
    (list width ascent descent (nreverse items))))

(defun org-typst-math-fractions--layout (source font &optional depth)
  "Recursively lay out SOURCE using FONT at nesting DEPTH.
Explicit groups establish fraction operands.  A depth bound keeps malformed
or extremely deeply nested input from doing unbounded redisplay work."
  (setq depth (or depth 0))
  (when (> depth 8) (throw 'unsupported nil))
  (with-temp-buffer
    (insert source)
    (with-syntax-table org-typst-math--script-syntax-table
      (when (and (eq (char-after (point-min)) ?\()
                 (eq (ignore-errors (scan-sexps (point-min) 1)) (point-max)))
        (delete-char -1)
        (goto-char (point-min)) (delete-char 1))
      (let ((fractions (org-typst-math-fractions--find (point-min) (point-max)))
            boxes)
        (goto-char (point-min))
        (while (< (point) (point-max))
          (cond
           ((and fractions (= (point) (caar fractions)))
            (let ((fraction (pop fractions)))
              (push (org-typst-math-fractions--fraction-box
                     (org-typst-math-fractions--layout (nth 2 fraction) font (1+ depth))
                     (org-typst-math-fractions--layout (nth 3 fraction) font (1+ depth))
                     font) boxes)
              (goto-char (nth 1 fraction))))
           ((eq (char-after) ?\()
            (let ((end (ignore-errors (scan-sexps (point) 1))))
              (unless end (throw 'unsupported nil))
              (push (org-typst-math-fractions--join
                     (list (org-typst-math-fractions--text-box "(" font)
                           (org-typst-math-fractions--layout
                            (buffer-substring-no-properties (1+ (point)) (1- end))
                            font (1+ depth))
                           (org-typst-math-fractions--text-box ")" font))) boxes)
              (goto-char end)))
           ((and org-typst-math-fractions--scripts
                 (memq (char-after) '(?_ ?^)))
            (let ((up (eq (char-after) ?^)))
              (forward-char) (skip-chars-forward " \t")
              (let* ((end (org-typst-math--script-end))
                     (base (car boxes)))
                (unless (and end base) (throw 'unsupported nil))
                (let* ((box (org-typst-math-fractions--layout
                             (buffer-substring-no-properties (point) end) font (1+ depth)))
                       (shift (if up
                                  (- (max (round (* (font-get font :size) 0.45))
                                          (- (nth 1 base) (nth 1 box))))
                                (max (round (* (font-get font :size) 0.3))
                                     (nth 2 base)))))
                  (push (list (car box) (max 0 (- (nth 1 box) shift))
                              (max 0 (+ (nth 2 box) shift))
                              (mapcar (lambda (item)
                                        (list (car item) (nth 1 item) (+ shift (nth 2 item))))
                                      (nth 3 box))) boxes)
                  (goto-char end)))))
           (t
            (let* ((start (point))
                   (stop (or (caar fractions) (point-max)))
                   (end (save-excursion
                          (if (re-search-forward
                               (if org-typst-math-fractions--scripts "[(_^]" "(")
                               stop t)
                              (match-beginning 0) stop))))
              (push (org-typst-math-fractions--text-box
                     (buffer-substring-no-properties start end) font) boxes)
              (goto-char end)))))
        (org-typst-math-fractions--join (nreverse boxes))))))

(defun org-typst-math-fractions--shape (gstring &optional _direction)
  "Lay out a generated fraction GSTRING using positioned text glyphs."
  (save-match-data
    (let* ((header (aref gstring 0))
           (font (aref header 0))
           (text (concat (cdr (append header nil)))))
      (when (and (fontp font)
                 (string-match "\\`!otmf\\([01]\\){\\([^|]+\\)|\\([^|]+\\)}!\\'" text))
        (let ((org-typst-math-fractions--scripts
               (equal (match-string 1 text) "1"))
              (num-source (match-string 2 text))
              (den-source (match-string 3 text)))
          (catch 'unsupported
            (let* ((box (org-typst-math-fractions--fraction-box
                         (org-typst-math-fractions--layout num-source font)
                         (org-typst-math-fractions--layout den-source font) font))
                   (glyphs (mapcar (lambda (item)
                                     (let ((glyph (car item)))
                                       (aset glyph 0 0)
                                       (aset glyph 1 (1- (length text)))
                                       (aset glyph 9 (vector (nth 1 item) (nth 2 item) 0))
                                       glyph))
                                   (nth 3 box))))
              ;; WADJUST is the resulting advance, not a delta to glyph width.
              (aset (aref (car (last glyphs)) 9) 2 (car box))
              (vconcat (vector header nil) glyphs))))))))

(defun org-typst-math-fractions--clear (beg end)
  "Remove experimental fraction overlays intersecting BEG through END."
  (setq org-typst-math-fractions--overlays
        (cl-delete-if
         (lambda (ov)
           (when (or (not (overlay-buffer ov))
                     (and (<= (overlay-start ov) end) (>= (overlay-end ov) beg)))
             (delete-overlay ov) t))
         org-typst-math-fractions--overlays)))

(defun org-typst-math-fractions--reveal (&optional position)
  "Show editable source for the fraction containing POSITION or point."
  (setq position (or position (point)))
  (dolist (ov org-typst-math-fractions--overlays)
    (overlay-put ov 'display
                 (unless (and (<= (overlay-start ov) position)
                              (<= position (overlay-end ov)))
                   (overlay-get ov 'fraction-display)))))

(defun org-typst-math-fractions--refresh (beg end)
  "Compose fractions in math fragments overlapping BEG through END."
  (org-typst-math-fractions--clear beg end)
  (when (and org-typst-math-pretty-fractions org-typst-math-mode
             org-pretty-entities (display-graphic-p))
    (save-excursion
      (dolist (range (org-typst-math--math-ranges))
        (when (and (< (car range) end) (> (cdr range) beg))
          (org-typst-math-fractions--clear (car range) (cdr range))
          (dolist (fraction (org-typst-math-fractions--find (car range) (cdr range)))
            (let* ((string (org-typst-math-fractions--string
                            (nth 2 fraction) (nth 3 fraction)))
                   (font (font-at (nth 0 fraction)
                                  (get-buffer-window (current-buffer) t))))
              ;; Check font coverage before replacing source with an encoded
              ;; display string: failed shaping must never expose the encoding.
              (when (and font
                         (org-typst-math-fractions--shape
                          (composition-get-gstring 0 (length string) font string)))
                (let ((ov (make-overlay (nth 0 fraction) (nth 1 fraction))))
                  (overlay-put ov 'fraction-display string)
                  (overlay-put ov 'evaporate t)
                  (push ov org-typst-math-fractions--overlays))))))))
    (org-typst-math-fractions--reveal
     (when (markerp org-typst-math--script-cursor)
       (marker-position org-typst-math--script-cursor)))))

(defun org-typst-math-fractions--changed (beg end _old)
  "Discard stale fraction displays after edits between BEG and END."
  (org-typst-math-fractions--clear beg end))

(defun org-typst-math-fractions--setup ()
  "Install buffer-local shaping and editing hooks once."
  (unless org-typst-math-fractions--installed
    (setq org-typst-math-fractions--local-table-p
          (local-variable-p 'composition-function-table)
          org-typst-math-fractions--composition-table composition-function-table)
    (setq-local composition-function-table (copy-sequence composition-function-table))
    (set-char-table-range
     composition-function-table ?!
     (cons '["!otmf[01]{[^|\n]+|[^|\n]+}!" 0 org-typst-math-fractions--shape]
           (aref composition-function-table ?!)))
    (setq org-typst-math-fractions--installed t)
    (clear-composition-cache)
    (add-hook 'after-change-functions #'org-typst-math-fractions--changed nil t)
    (add-hook 'post-command-hook #'org-typst-math-fractions--reveal nil t)
    (add-hook 'change-major-mode-hook #'org-typst-math-fractions--teardown nil t)))

(defun org-typst-math-fractions--teardown ()
  "Remove fraction displays and restore the buffer's composition table."
  (when org-typst-math-fractions--installed
    (remove-hook 'after-change-functions #'org-typst-math-fractions--changed t)
    (remove-hook 'post-command-hook #'org-typst-math-fractions--reveal t)
    (remove-hook 'change-major-mode-hook #'org-typst-math-fractions--teardown t)
    (org-typst-math-fractions--clear (point-min) (point-max))
    (if org-typst-math-fractions--local-table-p
        (setq composition-function-table org-typst-math-fractions--composition-table)
      (kill-local-variable 'composition-function-table))
    (setq org-typst-math-fractions--installed nil)))

(provide 'org-typst-math-fractions)
;;; org-typst-math-fractions.el ends here
