(define-module (toobusy toobusy)
  #:use-module (toobusy util)
  #:use-module (toobusy xdg)

  #:export (DTSTART_SLOT
            DTEND_SLOT
            get-database-path))

(define DTSTART_SLOT 0)
(define DTEND_SLOT 1)

(define (get-database-path)
  (path-join (xdg-dir 'data-home) "xapian.db"))
