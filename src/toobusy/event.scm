(define-module (toobusy event)
  #:use-module (toobusy util)
  #:use-module (toobusy time)
  #:use-module (toobusy toobusy)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)

  #:export (make-event
            event?
            event-docid
            event-rank
            event-ics
            event-start
            event-end
            event-before?
            event-after?
            event-print
            group-events))

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

(define (%event-tm prop-name event)
  (let* ((ics-obj  (event-ics event))
         (property (ics-object-property-ref ics-obj prop-name)))
    (ics-property-value
      (ics-property->typed-property property))))

(define (event-start event)
  (%event-tm "DTSTART" event))

(define (event-end event)
  (%event-tm "DTEND" event))

(define (event-before? event other)
  (tm-before? (event-start event) (event-start other)))

;; Check if the event is on the same date or after.
(define (event-after? event other)
  (not (event-before? event other)))

(define* (event-print event #:optional (port (current-output-port)))
  (let* ((ics-obj (event-ics event))
         (summary (ics-object-property-ref ics-obj "SUMMARY")))
    (format port
            "~a [~a] '~a'~%"
            (event-docid event)
            (event-rank event)
            (ics-property-value summary))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (group-events events)
  (fold
    (lambda (event xs)
      (if (null? xs)
        (cons (list event) xs)
        (let ((cur (event-start event))
              (prv (event-start (caar xs))))
          (if (eq? (tm:mday cur) (tm:mday prv))
            (cons (cons event (car xs)) (cdr xs))
            (cons (list event) xs)))))
    '() (sort events event-before?)))
