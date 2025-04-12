(define-module (toobusy search)
  #:use-module (toobusy util)
  #:use-module (toobusy event)
  #:use-module (toobusy toobusy)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)

  #:use-module (xapian xapian)
  #:use-module (xapian wrap)

  #:use-module (srfi srfi-1)

  #:export (make-query
            range-query
            search-query
            search))

(define (date-range slot prefix)
  (prefixed-date-range-processor
    0
    #:prefix (string-append prefix ":")
    #:repeated? #f
    #:prefer-mdy? #f))

(define (enquire-events db query)
  (mset-fold
    (lambda (item xs)
      (xcons
        xs
        (let* ((data    (document-data (mset-item-document item)))
               (ics-obj (car (ics->scm data))))
          (make-event
            (mset-item-docid item)
            (mset-item-rank item)
            ics-obj))))
    '() (enquire-mset (enquire db query)
                      #:maximum-items 50)))

(define (make-query querystr)
  (let* ((range (list (date-range DTSTART_SLOT "start")
                      (date-range DTEND_SLOT "end"))))
    (parse-query querystr
                 #:stemmer (make-stem "en")
                 #:range-processors range
                 #:prefixes '(("summary" . "S")))))

(define (range-query start-time end-time)
  (new-Query
    (Query-OP-VALUE-RANGE)
    DTSTART_SLOT
    (strftime "%Y%m%d" start-time)
    (strftime "%Y%m%d" end-time)))

(define (search-query query)
  (make-query query))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search query)
  (call-with-database (get-database-path)
    (lambda (db)
      (enquire-events db query))))
