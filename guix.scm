(use-modules
 (gnu packages)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages tls)

 (guix)
 (guix packages)
 (guix build-system guile)
 (guix build utils)
 (guix gexp)
 (guix git)
 (guix git-download)
 ((guix licenses) #:prefix license:)

 (ice-9 popen)
 (ice-9 textual-ports)
 )

(define %source-dir
  (dirname (current-filename)))

(define %git-commit
  (with-directory-excursion %source-dir
    (get-line (open-input-pipe "git rev-parse HEAD"))))

(define guile-quad-tree
  (package
    (name "guile-quad-tree")
    (version (git-version "0.1" "HEAD" %git-commit))
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments '(#:source-directory "."))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list ;; guile-fibers-1.1 guile-gnutls
                        ))
    (home-page "https://github.com/Halfwake/guile-quad-tree/")
    (synopsis "Guile quad-tree implementation")
    (description
     "Quad tree implementation for GNU Guile.")
    (license license:lgpl3+)))

guile-quad-tree
