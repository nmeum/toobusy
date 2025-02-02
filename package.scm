(use-modules (guix packages)
             (guix gexp)
             (guix build utils)
             (guix build-system gnu)
             (gnu packages guile-xyz)
             ((guix licenses) #:prefix license:))

(package
  (name "toobusy")
  (version "0.1.0")
  (source (local-file "." "git-checkout"
                      #:recursive? #t))
  (propagated-inputs
    (list
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
          (delete 'configure))))
  (synopsis "notmuch but for calendars")
  (description "")
  (home-page "https://git.8pit.net/toobusy")
   (license (list license:gpl3+)))
