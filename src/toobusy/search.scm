(define-module (toobusy search)
  #:use-module (toobusy util)
  #:use-module (toobusy)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)

  #:use-module (xapian xapian)
  #:export (find-entries
            search))

(define (date-range slot prefix)
  (prefixed-date-range-processor
    0
    #:prefix (string-append prefix ":")
    #:repeated? #f
    #:prefer-mdy? #f))

(define (find-entries db querystr)
  (let* ((range (list (date-range DTSTART_SLOT "start")
                      (date-range DTEND_SLOT "end")))
         (query (parse-query querystr
                             #:stemmer (make-stem "en")
                             #:range-processors range
                             #:prefixes '(("summary" . "S")))))
    (enquire-mset (enquire db query)
                  #:maximum-items 50)))

(define (print-entries db querystr)
  (let ((mset (find-entries db querystr)))
    (mset-fold (lambda (item _)
                 (let* ((data    (document-data (mset-item-document item)))
                        (event   (car (ics->scm data)))
                        (summary (ics-object-property-ref event "SUMMARY")))
                   (format #t "~a [#~3,'0d] '~a'~%"
                           (mset-item-rank item)
                           (mset-item-docid item)
                           (ics-property-value summary))))
               #f mset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search query)
  (let ((dbpath (get-database-path)))
    (call-with-database dbpath
      (lambda (db)
        (print-entries db query)))))
