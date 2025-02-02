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
