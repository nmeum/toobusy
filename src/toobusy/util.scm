(define-module (toobusy util)
  #:use-module (ice-9 copy-tree)
  #:use-module (ice-9 string-fun)
  #:use-module ((srfi srfi-1) #:select (fold fold-right))

  #:export (string-empty?
            path-join
            home-directory
            xdg-dir
            mkdir-p))

(define (string-empty? str)
  (zero? (string-length str)))

(define (path-join . elems)
  (fold-right
    (lambda (elem path)
      (if (string-empty? path)
        elem
        (string-append elem "/" path)))
    "" elems))

(define (home-directory)
  (or
    (getenv "HOME")
    (error "HOME environment variable is not set")))

(define (xdg-dir name)
  (define xdg-base-dirs
    (list
      (cons "XDG_DATA_HOME" (path-join ".local" "share"))
      (cons "XDG_CONFIG_HOME" (path-join ".config"))))

  (define (%xdg-dir name)
    (let ((v (assoc name xdg-base-dirs)))
      (if (not v)
        (error (format #f "unsupported xdg base directory ~s" name))
        (or
          (getenv (car v))
          (path-join (home-directory) (cdr v))))))

  (let* ((dir (path-join (%xdg-dir name) "toobusy")))
    (mkdir-p dir)
    dir))

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
