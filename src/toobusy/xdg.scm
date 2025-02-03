(define-module (toobusy xdg)
  #:use-module (toobusy util)

  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (xdg-dir))

;; See https://specifications.freedesktop.org/basedir-spec/latest/
(define xdg-base-dirs
  (list
    (cons "XDG_DATA_HOME" (path-join ".local" "share"))
    (cons "XDG_CONFIG_HOME" (path-join ".config"))))

(define (home-directory)
  (define home #f)

  (or
    home
    (or
      (begin
        (set! home (getenv "HOME"))
        home)
      (error "HOME environment variable is not set"))))

(define (%xdg-dir name)
  (let ((v (assoc name xdg-base-dirs)))
    (if (not v)
      (error (format #f "unsupported xdg base directory ~s" name))
      (or
        (getenv (car v))
        (path-join (home-directory) (cdr v))))))

(define (symbol->env name)
  (string-append
    "XDG_"
    (string-replace-substring
      (string-upcase (symbol->string name)) "-" "_")))

(define (xdg-dir name)
  (let* ((sym (symbol->env name))
         (dir (path-join (%xdg-dir sym) "toobusy")))
    (mkdir-p dir)
    dir))
