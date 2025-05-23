(define-module (toobusy config)
  #:use-module (toobusy util)
  #:use-module (srfi srfi-9)
  #:use-module ((scheme load) #:prefix load:)
  #:use-module ((scheme eval) #:prefix eval:)

  #:export (config?
            config
            config-date-format
            config-list-dayfmt
            config-list-heading
            config-list-delta

            %config
            load-config!)

  ;; Needed because of (load …) usage.
  #:declarative? #f)

(define-record-type <config>
  (make-config date-format list-dayfmt list-heading list-delta)
  config?
  (date-format config-date-format)
  (list-dayfmt config-list-dayfmt)
  (list-heading config-list-heading)
  (list-delta config-list-delta))

;; TODO: Define sanitizers.
(define* (config #:key
                 (date-format "%Y-%m-%d")
                 (list-dayfmt "%A (%a), %Y-%m-%d")
                 (list-heading '(BOLD))
                 (list-delta 7))
  (make-config date-format
               list-dayfmt
               list-heading
               (string-append "+" (number->string list-delta))))

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
