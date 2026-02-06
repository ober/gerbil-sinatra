(import ./response
        ./helpers
        ./context)

(export handle-not-found
        handle-error)

;; Handle 404 - not found
(def (handle-not-found app sres)
  (sinatra-response-status-set! sres 404)
  (let ((handler (hash-get app 'not-found-handler)))
    (if handler
      (let ((body (handler)))
        (when (string? body)
          (sinatra-response-body-set! sres body)))
      (sinatra-response-body-set! sres "<h1>Not Found</h1>"))))

;; Handle errors
(def (handle-error app sres exn)
  (let ((error-handlers (or (hash-get app 'error-handlers) (hash))))
    ;; Try catch-all error handler
    (let ((catch-all (hash-get error-handlers 'error)))
      (if catch-all
        (begin
          (sinatra-response-status-set! sres 500)
          (let ((body (parameterize ((current-error exn))
                        (catch-all))))
            (when (string? body)
              (sinatra-response-body-set! sres body))))
        ;; Default error handling
        (begin
          (sinatra-response-status-set! sres 500)
          (let ((environment (or (hash-get app 'environment) "development")))
            (if (string=? environment "development")
              (sinatra-response-body-set! sres
                (string-append "<h1>Error</h1><pre>"
                  (with-output-to-string (lambda () (display-exception exn)))
                  "</pre>"))
              (sinatra-response-body-set! sres
                "<h1>Internal Server Error</h1>"))))))))
