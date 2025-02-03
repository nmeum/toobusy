(define-module (toobusy util)
  #:use-module (ice-9 copy-tree)
  #:use-module ((srfi srfi-1) #:select (fold fold-right))

  #:export (string-empty?
            path-join
            mkdir-p
            mkzoned
            add-days))

(define (empty-string? str)
  (zero? (string-length str)))

(define (path-join . elems)
  (fold-right
    (lambda (elem path)
      (if (empty-string? path)
        elem
        (string-append elem "/" path)))
    "" elems))

;; TODO: Just copy the implementation from Guix.
(define (mkdir-p path)
  (unless (absolute-file-name? path)
    (error "mkdir-p only supports absolute file names"))

  (fold
    (lambda (x xs)
      (let ((new-path (path-join xs x)))
        (unless (file-exists? new-path)
          (mkdir new-path))
        new-path))
    "/" (string-split path #\/)))

(define (mkzoned tm)
  (mktime tm (or (tm:zone tm) "")))

(define (add-days tm days)
  (let* ((t (copy-tree tm)))
    (set-tm:mday t (+ days (tm:mday t)))
    (cdr (mkzoned t))))
