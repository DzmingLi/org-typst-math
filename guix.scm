;;; Build with: guix build -f guix.scm
;;; Install with: guix package -f guix.scm

(use-modules (guix packages)
             (guix gexp)
             (guix import crate)
             (guix build-system cargo)
             (guix build-system emacs)
             (gnu packages rust)
             (gnu packages emacs-xyz))

(define %project-root
  (dirname (canonicalize-path (current-filename))))

;; Only include build inputs, never .git, target/, local tests, or compiled Lisp.
;; This also works before new source files have been committed to Git.
(define %project-source
  (local-file %project-root "org-typst-math-source"
              #:recursive? #t
              #:select?
              (lambda (file stat)
                (or (string=? file %project-root)
                    (let ((relative (substring file (+ 1 (string-length %project-root)))))
                      (or (member relative '("Cargo.toml" "Cargo.lock" "helper" "lisp"))
                          (and (string-prefix? "helper/" relative)
                               (string-suffix? ".rs" relative))
                          (and (string-prefix? "lisp/" relative)
                               (string-suffix? ".el" relative))))))))

(define org-typst-math-helper
  (package
    (name "org-typst-math-helper")
    (version "0.1.0")
    (source %project-source)
    (build-system cargo-build-system)
    (arguments
     (list
      ;; Typst 0.15 requires Rust 1.92 or newer.
      #:rust rust-1.93
      #:install-source? #f))
    ;; Guix fetches checksum-verified crate sources before the offline build.
    ;; Cargo.lock is the only dependency list to maintain.
    (inputs (cargo-inputs-from-lockfile
             (string-append %project-root "/Cargo.lock")))
    (home-page "https://github.com/DzmingLi/org-typst-math")
    (synopsis "Persistent Typst conversion helper for Org")
    (description
     "This JSON-RPC helper converts Typst mathematics to SVG, MathML and LaTeX.
It retains compiler caches between requests and reports source diagnostics.")
    ;; The project has not declared a license; do not infer one from its dependencies.
    (license #f)))

(define emacs-org-typst-math
  (package
    (name "emacs-org-typst-math")
    (version (package-version org-typst-math-helper))
    (source %project-source)
    (build-system emacs-build-system)
    (arguments
     (list
      #:lisp-directory "lisp"
      ;; Integration checks are local and not distributed with the package.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-packaged-helper
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "org-typst-math.el"
                (("'\\(\"org-typst-math-helper\"\\)")
                 (string-append "'(\""
                                (search-input-file inputs "/bin/org-typst-math-helper")
                                "\")"))))))))
    (inputs (list org-typst-math-helper))
    (propagated-inputs (list emacs-org))
    (home-page (package-home-page org-typst-math-helper))
    (synopsis "Typst mathematics and inline previews in Org")
    (description
     "Write Typst mathematics inside Org's math delimiters, preview formulas
as SVG, and export them as MathML or LaTeX.  The matching conversion helper is
included as a runtime dependency and is selected by its absolute store path.")
    (license (package-license org-typst-math-helper))))

;; -f installs the Elisp package together with its runtime closure.
emacs-org-typst-math
