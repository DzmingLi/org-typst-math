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

;;;###autoload
(define-minor-mode org-typst-math-mode
  "Identify this buffer as using Typst mathematics.
Export is controlled by #+TYPST_MATH: t or the :typst-math export option.
This mode does not rewrite the document or enable image preview."
  :lighter " TypMath"
  (when (and org-typst-math-mode (not (derived-mode-p 'org-mode)))
    (setq org-typst-math-mode nil)
    (user-error "Typst math mode requires Org")))

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
