(define-module (toobusy index)
  #:use-module (toobusy util)

  #:use-module (ics)
  #:use-module (ics object)
  #:use-module (ics property)
  #:use-module (ics type property)

  #:use-module (xapian xapian)
  #:use-module ((srfi srfi-1) #:select (concatenate))

  #:export (index-files))

(define (index-vevents db events)
  (define (index-vevent event)
    (let* ((uid      (ics-object-property-ref event "UID"))
           (summary  (ics-object-property-ref event "SUMMARY"))
           (dtstart  (ics-object-property-ref event "DTSTART"))
           (dtend    (ics-object-property-ref event "DTEND"))

           (id-term  (string-append "Q" (ics-property-value uid)))
           (doc      (make-document #:data (scm->ics-string event)
                                    ;; TODO: Why 0?
                                    #:terms `((,id-term . 0))))
           (term-gen (make-term-generator
                       #:stem (make-stem "en")
                       #:document doc)))
      ;; Index each field with a suitable prefix.
      (index-text! term-gen (ics-property-value summary) #:prefix "S")

      ;; Index fields without prefixes for general search.
      (index-text! term-gen (ics-property-value summary))
      ;; (increase-termpos! term-gen)

      ;; Store the dtstart/dtend as values for a ValueRange search.
      (let ((date-time->string
              (lambda (prop)
                (let ((t (ics-property->typed-property prop)))
                  (strftime "%s" (ics-property-value t))))))
        (document-slot-set! doc 0 (date-time->string dtstart))
        (document-slot-set! doc 1 (date-time->string dtend)))

      ;; Actually add the document to the database.
      (format #t "indexed: ~a~%" doc)
      (replace-document! db id-term doc)))

  (for-each index-vevent events))

(define (get-events files)
  (define (process-file file-path)
    (let* ((port    (open-input-file file-path))
           (ics-obj (car (ics->scm port))))
      (ics-object-components ics-obj)))

  (concatenate (map process-file files)))

(define (index-files files)
  (let ((events (get-events files))
        (dbpath (get-database-path)))
    (call-with-writable-database dbpath
     (lambda (db)
       (index-vevents db events)))))
