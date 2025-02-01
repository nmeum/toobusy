(define-module (toobusy main)
  #:use-module (toobusy index)
  #:use-module (toobusy util)

  #:use-module (ics)
  #:use-module (ics object)

  #:use-module (xapian xapian)
  #:use-module ((srfi srfi-1) #:select (concatenate))

  #:export (data-directory
            get-database-path
            get-events
            main))

(define (data-directory)
  (define (home)
    (or (getenv "HOME")
        (error "HOME environment variable is not set")))

  (let* ((data-home (or
                      (getenv "XDG_DATA_HOME")
                      (path-join (home) ".local" "share"))))
    (path-join data-home "toobusy")))

(define (get-database-path)
  (let ((data-dir (data-directory)))
    (mkdir-p data-dir)
    (path-join data-dir "xapian.db")))

(define (get-events files)
  (define (process-file file-path)
    (let* ((port    (open-input-file file-path))
           (ics-obj (car (ics->scm port))))
      (ics-object-components ics-obj)))

  (concatenate (map process-file files)))

(define (main files)
  (let ((events (get-events files))
        (dbpath (get-database-path)))
    (call-with-writable-database dbpath
     (lambda (db)
       (index-vevents db events)))))
