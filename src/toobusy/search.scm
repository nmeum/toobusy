(define-module (toobusy search)
  #:use-module (toobusy util)

  #:use-module (xapian xapian)
  #:export (find-entries
            search))

(define (find-entries db querystr)
  (let* ((query (parse-query querystr
                             #:stemmer (make-stem "en")
                             #:prefixes '(("summary" . "S")))))
    (enquire-mset (enquire db query)
                  #:maximum-items 50)))

(define (print-entries db querystr)
  (let ((mset (find-entries db querystr)))
    (mset-fold (lambda (item _)
                 (format #t "~a [#~3,'0d]~%"
                         (mset-item-rank item)
                         (mset-item-docid item)))
               #f mset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search query)
  (let ((dbpath (get-database-path)))
    (call-with-writable-database dbpath
      (lambda (db)
        (print-entries db query)))))
