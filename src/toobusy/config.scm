(define-module (toobusy config)
  #:use-module (toobusy util)
  #:use-module (srfi srfi-9)
  #:use-module ((scheme load) #:prefix load:)
  #:use-module ((scheme eval) #:prefix eval:)

  #:export (config?
            config
            config-date-format

            %config
            load-config!)

  ;; Needed because of (load …) usage.
  #:declarative? #f)

(define-record-type <config>
  (make-config date-format)
  config?
  (date-format config-date-format))

(define* (config #:key
                 (date-format "%Y-%m-%d"))
  (make-config date-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define %config (config))

(define* (load-config! #:optional path)
  (define (default-configuration-path)
    (path-join (xdg-dir "XDG_CONFIG_HOME") "config.scm"))

  ;; TODO: Load the Scheme source in the context of
  ;; a custom user-module, see Guix's load* procedure.
  ;;
  ;; See also: R7RS environments.
  (let ((config-path (or path (default-configuration-path))))
    (when (file-exists? config-path)
      (let* ((e (eval:environment '(toobusy config)))
             (r (load:load config-path e)))
        (if (config? r)
          (set! %config r)
          (error "configuration file must return a <config>"))))))
