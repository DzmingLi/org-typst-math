;;; check.el --- Guix package integration checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-typst-math)
(setq org-export-use-babel nil)

(ert-deftest org-typst-guix-helper-path ()
  (should (file-name-absolute-p (car typst-client-command)))
  (should (file-executable-p (car typst-client-command))))

(ert-deftest org-typst-guix-html-and-latex ()
  (let ((document "#+TYPST_MATH: t\n#+TYPST_HEADER: #let vect(x) = math.bold(x)\n$vect(v)$\n"))
    (should (string-match-p "<math" (org-export-string-as document 'html t)))
    (should (string-match-p (regexp-quote "\\mathbf")
                            (org-export-string-as document 'latex t)))))

(ert-deftest org-typst-guix-svg-and-error-recovery ()
  (let* ((result (typst-client-convert
                  default-directory ""
                  '((:id "bad" :source "unknownmacro(x)" :display :json-false)
                    (:id "good" :source "alpha" :display :json-false)) "svg"))
         (items (plist-get result :items)))
    (should (seq-some (lambda (d) (equal (plist-get d :severity) "error"))
                      (plist-get (aref items 0) :diagnostics)))
    (should (string-match-p "<svg"
                            (plist-get (plist-get (aref items 1) :artifact) :svg)))
    (should (jsonrpc-running-p (typst-client--connection default-directory)))))

(add-hook 'kill-emacs-hook #'typst-client-stop)
;;; check.el ends here
