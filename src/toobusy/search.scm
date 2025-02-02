(define-module (toobusy search)
  #:use-module (toobusy util)
  #:use-module (toobusy)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)

  #:use-module (xapian xapian)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)

  #:export (find-events
            search

            event?
            event-docid
            event-rank
            event-ics
            event-print))

;; TODO: If we want/need the file path, then we need to store that in the
;; database. However, since multiple VEVENTs may be stored in the same file it
;; is not sufficient to only store the file path in the database. Contrary to
;; notmuch where each maildir entry has a unique file path.
(define-record-type event
  (make-event docid rank ics)
  event?
  (docid event-docid)
  (rank  event-rank)
  (ics   event-ics))

(define* (event-print event #:optional (port (current-output-port)))
  (let* ((ics-obj (event-ics event))
         (summary (ics-object-property-ref ics-obj "SUMMARY")))
    (format port
            "~a [#~3,'0d] '~a'~%"
            (event-docid event)
            (event-rank event)
            (ics-property-value summary))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
