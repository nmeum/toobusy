(use-modules (guix packages)
             (guix gexp)
             (guix build utils)
             (guix build-system gnu)
             (gnu packages guile)
             (gnu packages guile-xyz)
             ((guix licenses) #:prefix license:))

(package
  (name "toobusy")
  (version "0.1.0")
  (source (local-file "." "git-checkout"
                      #:recursive? #t))
  (propagated-inputs
    (list
      guile-3.0
      guile-xapian
      guile-ics))
  (build-system gnu-build-system)
  (arguments
    (list
      #:tests? #f
      #:make-flags
      #~(list
          (string-append "PREFIX = " #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'setenv
            (lambda _
              (setenv "GUILE" (which "guile"))))
          (delete 'configure))))
  (synopsis "notmuch but for calendars")
  (description "")
  (home-page "https://git.8pit.net/toobusy")
   (license (list license:gpl3+)))
