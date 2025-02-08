(define-module (toobusy toobusy)
  #:use-module (toobusy util)

  #:export (DTSTART_SLOT
            DTEND_SLOT
            get-database-path))

(define DTSTART_SLOT 0)
(define DTEND_SLOT 1)

(define (get-database-path)
  (path-join (xdg-dir "XDG_DATA_HOME") "xapian.db"))
