;;; org-typst-math-export.el --- Export adapters for Typst math -*- lexical-binding: t; -*-

;; Part of the org-typst-math package; load through org-typst-math.el.

;;; Commentary:
;; Derived exporters share the ordinary Org export entry points.  HTML
;; and LaTeX send one batch to the helper.  Typst preserves math source and uses ox-typst's
;; existing TYPST_HEADER support.

;;; Code:
(require 'ox-html)
(require 'ox-latex)
(require 'typst-client)
(require 'compile)

(declare-function org-typst-math-fragment "org-typst-math" (element))
(declare-function org-typst-math--enabled-p "org-typst-math" (value))
(declare-function org-typst-latex-fragment "ox-typst" (fragment contents info))

(defvar org-typst-math-block-alist)
(declare-function org-typst-special-block "ox-typst" (block contents info))
(declare-function org-typst-link "ox-typst" (link contents info))
(declare-function org-typst--as-string "ox-typst" (value))

(defun org-typst-math--block-name (block)
  "Return the configured export name of BLOCK."
  (cdr (assoc (org-element-property :type block) org-typst-math-block-alist)))

(defun org-typst-math--block-arguments (block)
  "Read BLOCK's ATTR_TYPST pairs, retaining raw Typst expressions.
Only top-level :keys separate arguments; strings and brackets retain colons."
  (let* ((text (mapconcat #'identity (org-element-property :attr_typst block) " "))
         (length (length text)) (i 0) (depth 0) quoted key start pairs)
    (while (< i length)
      (let ((char (aref text i)))
        (cond
         ((and quoted (= char ?\\)) (setq i (1+ i)))
         ((= char ?\") (setq quoted (not quoted)))
         (quoted)
         ((memq char '(?\( ?\[ ?\{)) (cl-incf depth))
         ((memq char '(?\) ?\] ?\})) (cl-decf depth))
         ((and (= depth 0) (= char ?:)
               (or (= i 0) (memq (aref text (1- i)) '(?\s ?\t ?\n)))
               (string-match "\\`:\\([-[:alnum:]_]+\\)\\(?:[ \t\n]+\\|\\'\\)"
                             (substring text i)))
          (when key (push (cons key (string-trim (substring text start i))) pairs))
          (setq key (match-string 1 (substring text i))
                start (+ i (match-end 0)) i (1- start)))))
      (cl-incf i))
    (when key (push (cons key (string-trim (substring text start))) pairs))
    (nreverse pairs)))

(defun org-typst-math--renamed-block (block contents info translator)
  "Call TRANSLATOR on BLOCK with its mapped name, CONTENTS and INFO."
  (let ((original (org-element-property :type block)))
    (unwind-protect
        (progn
          (when-let* ((name (org-typst-math--block-name block)))
            (org-element-put-property block :type name))
          (funcall translator block contents info))
      (org-element-put-property block :type original))))

(defun org-typst-math--html-block (block contents info)
  "Export mapped BLOCK with CONTENTS and INFO through Org HTML."
  (org-typst-math--renamed-block block contents info #'org-html-special-block))

(defun org-typst-math--latex-block (block contents info)
  "Export mapped BLOCK with CONTENTS and INFO through Org LaTeX."
  (org-typst-math--renamed-block block contents info #'org-latex-special-block))

(defun org-typst-math--block-label (block info)
  "Return the Typst label expression for BLOCK using export INFO."
  (or (cdr (assoc "label" (org-typst-math--block-arguments block)))
      (format "label(%s)" (org-typst--as-string (org-export-get-reference block info)))))

(defun org-typst-math--typst-block (block contents info)
  "Export mapped BLOCK as a function call with CONTENTS and INFO."
  (if-let* ((name (org-typst-math--block-name block)))
      (let* ((pairs (org-typst-math--block-arguments block))
             (args (mapconcat (lambda (pair)
                                (if (equal (car pair) "positional") (cdr pair)
                                  (concat (car pair) ": " (cdr pair))))
                              pairs ", ")))
        (concat "#" name (unless (string-empty-p args) (concat "(" args ")"))
                "[\n" contents "]"
                (when (and (org-element-property :name block) (not (assoc "label" pairs)))
                  (concat " #" (org-typst-math--block-label block info))) "\n"))
    (org-typst-special-block block contents info)))

(defun org-typst-math--typst-link (link contents info)
  "Resolve LINK to mapped blocks using their label expressions and INFO."
  (let ((target (when (equal (org-element-property :type link) "fuzzy")
                  (org-export-resolve-fuzzy-link link info))))
    (if (and (org-element-type-p target 'special-block)
             (org-typst-math--block-name target))
        (let ((label (org-typst-math--block-label target info)))
          (if contents (format "#link(%s)[%s]" label contents)
            (format "#ref(%s)" label)))
      (org-typst-link link contents info))))

(defun org-typst-math--route-export (original backend &optional subtree visible body ext)
  "Route ORIGINAL export of BACKEND when Typst math is explicitly enabled.
SUBTREE, VISIBLE, BODY and EXT retain their Org meanings."
  (let* ((name (if (symbolp backend) backend (org-export-backend-name backend)))
         (enabled (and (memq name '(html typst latex))
                       (org-typst-math--enabled-p
                        (plist-get (org-export-get-environment backend subtree ext)
                                   :typst-math)))))
    (when enabled
      (setq backend
            (pcase name
              ('html 'typst-math-html)
              ('typst 'typst-math-typst)
              ('latex 'typst-math-latex))))
    (funcall original backend subtree visible body ext)))

(advice-add 'org-export-as :around #'org-typst-math--route-export)

(defun org-typst-math--byte-chars (text offset)
  "Convert UTF-8 byte OFFSET in TEXT to a character count."
  (length (decode-coding-string
           (substring (encode-coding-string text 'utf-8-unix) 0 offset)
           'utf-8-unix)))

(defun org-typst-math--location (diagnostic element fragment preamble)
  "Map DIAGNOSTIC to an Org or imported-file location.
ELEMENT, FRAGMENT and PREAMBLE describe the export snapshot."
  (let ((offset (or (plist-get diagnostic :start) 0)))
    (pcase (plist-get diagnostic :origin)
      ("file" (list (plist-get diagnostic :file) (plist-get diagnostic :line)
                    (or (plist-get diagnostic :column) 1)))
      ('nil (list "<Typst>" 1 1))
      (_
       (let ((position
              (if (equal (plist-get diagnostic :origin) "preamble")
                  (save-excursion
                    (goto-char (point-min))
                    (let* ((before (substring preamble 0 (org-typst-math--byte-chars preamble offset)))
                           (line (1+ (cl-count ?\n before)))
                           (column (length (car (last (split-string before "\n"))))))
                      (dotimes (_ line) (re-search-forward "^[ \t]*#\\+TYPST_HEADER:[ \t]*" nil t))
                      (+ (point) column)))
                (+ (org-element-property :begin element)
                   (plist-get fragment :offset)
                   (org-typst-math--byte-chars (plist-get fragment :source) offset)))))
         (save-excursion
           (goto-char position)
           (list (or buffer-file-name "<Org export>")
                 (line-number-at-pos) (1+ (- (point) (line-beginning-position))))))))))

(defun org-typst-math--diagnostics (entries)
  "Display diagnostic ENTRIES as compilation messages."
  (with-current-buffer (get-buffer-create "*Org Typst diagnostics*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (entry entries)
        (pcase-let ((`(,file ,line ,column ,severity ,message) entry))
          (insert (format "%s:%d:%d: %s: %s\n" file line column severity message)))))
    (compilation-mode))
  (when (seq-some (lambda (entry) (equal (nth 3 entry) "error")) entries)
    (display-buffer "*Org Typst diagnostics*")))

(defun org-typst-math--prepare (tree backend info)
  "Batch-convert math in TREE for BACKEND, saving artifacts on its nodes.
INFO contains this export's preamble and document settings."
  (let ((preamble (or (plist-get info :typst-header) ""))
        (htmlp (org-export-derived-backend-p backend 'html))
        elements fragments items)
    (org-element-map tree 'latex-fragment
      (lambda (element)
        (when-let* ((fragment (org-typst-math-fragment element)))
          (push element elements)
          (push fragment fragments)
          (push (list :id (number-to-string (length elements))
                      :source (plist-get fragment :source)
                      :display (plist-get fragment :display)) items))))
    (setq elements (nreverse elements) fragments (nreverse fragments) items (nreverse items))
    (when items
      (let* ((result (typst-client-convert default-directory preamble items
                                          (if htmlp "mathml" "latex")))
             (results (plist-get result :items))
             css entries failed)
        (cl-mapc
         (lambda (element fragment result)
           (let ((artifact (plist-get result :artifact)))
             (if artifact
                 (progn
                   (org-element-put-property element :typst-artifact artifact)
                   (when htmlp (cl-pushnew (plist-get artifact :css) css :test #'equal)))
               (setq failed t)))
           (seq-doseq (diagnostic (plist-get result :diagnostics))
             (cl-pushnew (append (org-typst-math--location diagnostic element fragment preamble)
                                 (list (plist-get diagnostic :severity) (plist-get diagnostic :message)))
                         entries :test #'equal)))
         elements fragments (append results nil))
        (org-typst-math--diagnostics (nreverse entries))
        (when failed (user-error "Typst compilation failed; see *Org Typst diagnostics*"))
        (when htmlp
          ;; INFO already contains :html-head-extra, so update it in place.
          (plist-put info :html-head-extra
                   (concat (plist-get info :html-head-extra)
                           "\n<style>\n" (mapconcat #'identity (nreverse css) "\n") "\n</style>"))
          (plist-put info :typst-math-css (mapconcat #'identity css "\n")))))
    tree))

(defun org-typst-math--html-body (body _backend info)
  "Keep MathML styles when exporting only BODY, using export INFO."
  (if (and (memq 'body-only (plist-get info :export-options))
           (plist-get info :typst-math-css))
      (concat "<style>" (plist-get info :typst-math-css) "</style>\n" body)
    body))

(defun org-typst-math--html-fragment (fragment contents info)
  "Export math FRAGMENT to MathML, or delegate non-math to Org.
CONTENTS and INFO are the standard translator arguments."
  (if-let* ((artifact (org-element-property :typst-artifact fragment)))
      (plist-get artifact :mathml)
    (org-html-latex-fragment fragment contents info)))

(defun org-typst-math--latex-fragment (fragment contents info)
  "Export FRAGMENT as LaTeX, delegating non-math with CONTENTS and INFO."
  (if-let* ((artifact (org-element-property :typst-artifact fragment)))
      (let ((body (plist-get artifact :latex))
            (display (eq (plist-get (org-typst-math-fragment fragment) :display) t)))
        ;; Org 9.8 wraps inline fragments in its latex-math-block node.
        (if display (concat "\\[\n" body "\n\\]") body))
    (org-latex-latex-fragment fragment contents info)))

(defun org-typst-math--latex-template (contents info)
  "Export CONTENTS using Org's template and required math packages from INFO."
  (let ((org-latex-packages-alist (copy-tree org-latex-packages-alist)))
    (dolist (name '("amsmath" "amssymb"))
      (unless (or (seq-some (lambda (entry) (equal (nth 1 entry) name))
                            (append org-latex-default-packages-alist org-latex-packages-alist))
                  (string-match-p (regexp-quote (concat "{" name "}"))
                                  (or (plist-get info :latex-header) "")))
        (push (list "" name t) org-latex-packages-alist)))
    (org-latex-template contents info)))

(defun org-typst-math--typst-fragment (fragment contents info)
  "Export FRAGMENT as Typst math, delegating non-math with CONTENTS and INFO."
  (if-let* ((math (org-typst-math-fragment fragment)))
      (format "#math.equation(block: %s, $%s$.body)"
              (if (eq (plist-get math :display) t) "true" "false")
              (plist-get math :source))
    (org-typst-latex-fragment fragment contents info)))

(org-export-define-derived-backend 'typst-math-html 'html
  :options-alist '((:typst-header "TYPST_HEADER" nil nil newline)
                   (:typst-math-css nil nil nil)
                   (:html-mathjax-template nil nil ""))
  :translate-alist '((latex-fragment . org-typst-math--html-fragment)
                     (special-block . org-typst-math--html-block))
  :filters-alist '((:filter-parse-tree . org-typst-math--prepare)
                   (:filter-body . org-typst-math--html-body)))

(org-export-define-derived-backend 'typst-math-latex 'latex
  :options-alist '((:typst-header "TYPST_HEADER" nil nil newline))
  :translate-alist '((latex-fragment . org-typst-math--latex-fragment)
                     (template . org-typst-math--latex-template)
                     (special-block . org-typst-math--latex-block))
  :filters-alist '((:filter-parse-tree . org-typst-math--prepare)))

(with-eval-after-load 'ox-typst
  (org-export-define-derived-backend 'typst-math-typst 'typst
    :translate-alist '((latex-fragment . org-typst-math--typst-fragment)
                       (special-block . org-typst-math--typst-block)
                       (link . org-typst-math--typst-link))))

(provide 'org-typst-math-export)
;;; org-typst-math-export.el ends here
