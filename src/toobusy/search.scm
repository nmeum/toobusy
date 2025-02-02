(define-module (toobusy search)
  #:use-module (toobusy util)
  #:use-module (toobusy event)
  #:use-module (toobusy)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)

  #:use-module (xapian xapian)
  #:use-module (srfi srfi-1)

  #:export (find-events
            search))

(define (date-range slot prefix)
  (prefixed-date-range-processor
    0
    #:prefix (string-append prefix ":")
    #:repeated? #f
    #:prefer-mdy? #f))

(define (find-events db querystr)
  (let* ((range (list (date-range DTSTART_SLOT "start")
                      (date-range DTEND_SLOT "end")))
         (query (parse-query querystr
                             #:stemmer (make-stem "en")
                             #:range-processors range
                             #:prefixes '(("summary" . "S")))))
    (mset-fold (lambda (item xs)
                 (xcons
                   xs
                   (let* ((data    (document-data (mset-item-document item)))
                          (ics-obj (car (ics->scm data))))
                     (make-event
                       (mset-item-docid item)
                       (mset-item-rank item)
                       ics-obj))))
               '() (enquire-mset (enquire db query)
                                #:maximum-items 50))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search query)
  (let ((dbpath (get-database-path)))
    (call-with-database dbpath
      (lambda (db)
        (find-events db query)))))
