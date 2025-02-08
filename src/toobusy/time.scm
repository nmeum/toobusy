(define-module (toobusy time)
  #:use-module (toobusy config)
  #:use-module (toobusy util)
  #:use-module (ice-9 copy-tree)

  #:export (mkzoned
            tm->time
            tm->string
            add-days
            tm-before?
            string->tm
            reltime->tm))

(define (mkzoned tm)
  (mktime tm (or (tm:zone tm) "")))

(define (tm->time tm)
  (car (mkzoned tm)))

(define (tm->string tm)
  (strftime (config-date-format %config) tm))

(define (add-days tm days)
  (let* ((t (copy-tree tm)))
    (set-tm:mday t (+ days (tm:mday t)))
    (cdr (mkzoned t))))

(define (tm-before? tm1 tm2)
  (let ((time1 (tm->time tm1))
        (time2 (tm->time tm2)))
    (< time1 time2)))

(define (string->tm str)
  (car (strptime (config-date-format %config) str)))

(define (reltime->tm relto str)
  (if (or (string-empty? str)
          (not (eq? #\+ (string-ref str 0))))
    #f
    (let ((n (string->number str)))
      (and n (add-days relto (string->number str))))))
