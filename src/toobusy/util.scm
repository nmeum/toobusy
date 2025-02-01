(define-module (toobusy util)
  #:use-module ((srfi srfi-1) #:select (fold fold-right))
  #:export (string-empty?
            path-join
            mkdir-p))

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
