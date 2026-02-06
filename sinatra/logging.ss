(import :std/format
        ./context
        ./request
        ./response)

(export log-request
        sinatra-logger
        current-logger)

;; Parameter for logger output port
(def current-logger (make-parameter (current-output-port)))

;; Log a completed request
(def (log-request method path status duration-ms)
  (let ((port (current-logger)))
    (fprintf port "~a ~a ~a (~ams)~n" method path status duration-ms)))

;; Middleware that times and logs each request
(def (sinatra-logger handler)
  (lambda (req res)
    (let ((start (current-milliseconds)))
      (handler req res)
      (let ((duration (- (current-milliseconds) start))
            (sreq (current-request))
            (sres (current-response)))
        (when (and sreq sres)
          (log-request
            (sinatra-request-method sreq)
            (sinatra-request-path sreq)
            (sinatra-response-status sres)
            duration))))))

;; Get current time in milliseconds
(def (current-milliseconds)
  (exact (floor (* (time->seconds (current-time)) 1000))))
