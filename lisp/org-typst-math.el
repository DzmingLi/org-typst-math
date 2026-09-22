;;; org-typst-math.el --- Typst mathematics in Org -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2") (org "9.7.11"))
;; Keywords: outlines, tex

;;; Commentary:
;; Enable exports with #+TYPST_MATH: t and define macros with #+TYPST_HEADER:.
;; Requiring this package integrates the standard HTML and optional ox-typst
;; export commands.  Inline previews are included and enabled separately.

;;; Code:
(require 'org)
(require 'ox)
(require 'typst-client)

(defgroup org-typst-math nil "Typst mathematics in Org." :group 'org)

(defcustom org-typst-math-block-alist nil
  "Map Org special block names to Typst functions and export environments.
Only listed blocks are converted.  No prefixes are stripped automatically."
  :type '(alist :key-type string :value-type string)
  :group 'org-typst-math)

(defcustom org-typst-math-entities
  '(;; Lowercase Greek letters, as spelled in Typst math.
    ("alpha" . "α") ("beta" . "β") ("gamma" . "γ") ("delta" . "δ")
    ("epsilon" . "ε") ("zeta" . "ζ") ("eta" . "η") ("theta" . "θ")
    ("iota" . "ι") ("kappa" . "κ") ("lambda" . "λ") ("mu" . "μ")
    ("nu" . "ν") ("xi" . "ξ") ("omicron" . "ο") ("pi" . "π")
    ("rho" . "ρ") ("sigma" . "σ") ("tau" . "τ") ("upsilon" . "υ")
    ("phi" . "φ") ("chi" . "χ") ("psi" . "ψ") ("omega" . "ω")
    ;; Uppercase Greek letters, as spelled in Typst math.
    ("Alpha" . "Α") ("Beta" . "Β") ("Gamma" . "Γ") ("Delta" . "Δ")
    ("Epsilon" . "Ε") ("Zeta" . "Ζ") ("Eta" . "Η") ("Theta" . "Θ")
    ("Iota" . "Ι") ("Kappa" . "Κ") ("Lambda" . "Λ") ("Mu" . "Μ")
    ("Nu" . "Ν") ("Xi" . "Ξ") ("Omicron" . "Ο") ("Pi" . "Π")
    ("Rho" . "Ρ") ("Sigma" . "Σ") ("Tau" . "Τ") ("Upsilon" . "Υ")
    ("Phi" . "Φ") ("Chi" . "Χ") ("Psi" . "Ψ") ("Omega" . "Ω")
    ;; Common operators and relations.
    ("integral" . "∫") ("sum" . "∑") ("dif" . "d")
    ("compose" . "∘") ("dot" . "⋅") ("eq.not" . "≠")
    ("tack.r" . "⊢") ("in" . "∈") ("forall" . "∀")
    ("exists" . "∃") ("times" . "×") ("infinity" . "∞"))
  "Alist mapping Typst math identifiers to Unicode replacements.
Used together with `org-pretty-entities' and `org-typst-math-mode': when
both are enabled, these identifiers are displayed as Unicode characters
inside Typst math fragments.  This affects display only; the source text,
previews, and exports keep the original Typst identifiers.  Add entries to
display further Typst symbols; for instance, map partial to ∂."
  :type '(alist :key-type string :value-type string)
  :group 'org-typst-math)

(defun org-typst-math-fragment (element)
  "Return (:source STRING :display BOOL :offset N) for math ELEMENT.
N is the delimiter length in characters.  Return nil for non-math LaTeX."
  (let ((value (org-element-property :value element)))
    (when value
      (cl-loop for (open close display) in
               '(("$$" "$$" t) ("\\[" "\\]" t)
                 ("\\(" "\\)" nil) ("$" "$" nil))
               when (and (string-prefix-p open value)
                         (string-suffix-p close value)
                         (>= (length value) (+ (length open) (length close))))
               return (list :source (substring value (length open) (- (length close)))
                            :display (if display t :json-false)
                            :offset (length open))))))

(defun org-typst-math--enabled-p (value)
  "Whether export option VALUE enables Typst math."
  (or (eq value t) (and (stringp value) (equal (downcase (string-trim value)) "t"))))

(defvar org-typst-math-mode)

(defconst org-typst-math--identifier-re
  "[[:alpha:]][[:alnum:]]*\\(?:\\.[[:alpha:]][[:alnum:]]*\\)*"
  "Regexp matching a candidate Typst math identifier and its variants.")

(defvar-local org-typst-math--math-ranges nil
  "Cache of Typst math fragment ranges.
Value is (KEY . RANGES), where KEY describes the buffer state and RANGES
is a list of (BEG . END) spans.")

(defun org-typst-math--math-ranges ()
  "Return the cached (BEG . END) ranges of Typst math fragments.
The cache is refreshed only when the buffer text, narrowing, or modification
tick changes, so font-lock never parses per identifier."
  (let ((key (list (buffer-chars-modified-tick) (point-min) (point-max))))
    (unless (equal (car-safe org-typst-math--math-ranges) key)
      (setq org-typst-math--math-ranges
            (cons key
                  (let (ranges)
                    (org-element-map (org-element-parse-buffer) 'latex-fragment
                      (lambda (element)
                        (when (org-typst-math-fragment element)
                          (push (cons (org-element-property :begin element)
                                      (org-element-property :end element))
                                ranges))
                        nil))
                    (nreverse ranges)))))
    (cdr org-typst-math--math-ranges)))

(defun org-typst-math--fontify-entities (limit)
  "Compose Typst math entities before LIMIT.
A font-lock matcher that follows `org-pretty-entities' and only runs in
`org-typst-math-mode' buffers.  Only identifiers listed in
`org-typst-math-entities' are composed, and only inside math fragments."
  (when (and org-typst-math-mode
             org-pretty-entities
             org-typst-math-entities)
    (catch 'match
      (dolist (range (org-typst-math--math-ranges))
        (when (and (< (point) (cdr range))
                   (< (car range) limit))
          (goto-char (max (point) (car range)))
          (while (re-search-forward org-typst-math--identifier-re
                                    (min limit (cdr range)) t)
            (let ((beg (match-beginning 0))
                  (end (match-end 0))
                  (entry (assoc (match-string 0) org-typst-math-entities)))
              (when (and entry
                         ;; Leave \alpha to `org-fontify-entities'.
                         (not (eq (char-before beg) ?\\)))
                (add-text-properties beg end '(font-lock-fontified t))
                (compose-region beg end (cdr entry) nil)
                (throw 'match t))))))
      nil)))

(defun org-typst-math--font-lock-keywords ()
  "Install the Typst entity matcher into Org's font-lock keywords."
  (setq org-font-lock-extra-keywords
        (append org-font-lock-extra-keywords
                '(org-typst-math--fontify-entities))))

(add-hook 'org-font-lock-set-keywords-hook #'org-typst-math--font-lock-keywords)

(defun org-typst-math--clear-pretty-entities ()
  "Remove compositions installed by `org-typst-math--fontify-entities'.
Re-fontify afterwards so that compositions from `org-fontify-entities'
are restored."
  (when (derived-mode-p 'org-mode)
    (with-silent-modifications
      (decompose-region (point-min) (point-max)))
    (org-restart-font-lock)))

;;;###autoload
(define-minor-mode org-typst-math-mode
  "Identify this buffer as using Typst mathematics.
Export is controlled by #+TYPST_MATH: t or the :typst-math export option.
This mode does not rewrite the document or enable image preview."
  :lighter " TypMath"
  (cond
   ((not (derived-mode-p 'org-mode))
    (setq org-typst-math-mode nil)
    (user-error "Typst math mode requires Org"))
   (org-typst-math-mode
    (when org-pretty-entities (font-lock-flush)))
   (t (org-typst-math--clear-pretty-entities))))

(defun org-typst-math--setup ()
  "Enable the editing mode when the current Org file declares Typst math."
  (when (org-typst-math--enabled-p
         (cadr (assoc "TYPST_MATH" (org-collect-keywords '("TYPST_MATH")))))
    (org-typst-math-mode 1)))

(add-hook 'org-mode-hook #'org-typst-math--setup)

;; Backend-independent so ordinary export commands can see the declaration.
(add-to-list 'org-export-options-alist '(:typst-math "TYPST_MATH" nil nil t))

(autoload 'org-typst-preview "org-typst-math-preview" nil t)
(autoload 'org-typst-math-preview-refresh "org-typst-math-preview" nil t)
(autoload 'org-typst-math-preview-clear "org-typst-math-preview" nil t)
(autoload 'org-typst-math-preview-diagnostics "org-typst-math-preview" nil t)

(require 'org-typst-math-export)

(provide 'org-typst-math)

(autoload 'org-typst-math-preview--setup-backend "org-typst-math-preview")

(with-eval-after-load 'org-fragtog-plus
  (add-hook 'org-typst-math-mode-hook #'org-typst-math-preview--setup-backend)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when org-typst-math-mode
        (org-typst-math-preview--setup-backend)))))
;;; org-typst-math.el ends here
