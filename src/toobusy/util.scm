(define-module (toobusy util)
  #:use-module ((srfi srfi-1) #:select (fold fold-right))
  #:export (string-empty?
            path-join
            mkdir-p
            data-directory
            get-database-path))

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

(define (data-directory)
  (define (home)
    (or (getenv "HOME")
        (error "HOME environment variable is not set")))

  (let* ((data-home (or
                      (getenv "XDG_DATA_HOME")
                      (path-join (home) ".local" "share"))))
    (path-join data-home "toobusy")))

(define (get-database-path)
  (let ((data-dir (data-directory)))
    (mkdir-p data-dir)
    (path-join data-dir "xapian.db")))
