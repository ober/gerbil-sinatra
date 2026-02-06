(import :std/test
        :std/text/json
        :std/net/request
        :std/net/httpd
        :std/format
        ./app
        ./handler
        ./context
        ./helpers
        ./dsl)

(export handler-test)

;; Find a free port for testing
(def (find-free-port)
  (+ 10000 (random-integer 50000)))

;; Start a test server, run body, stop server
(def (with-test-server app proc)
  (let* ((port (find-free-port))
         (handler-fn (sinatra-handler app))
         (mux (make-default-http-mux handler-fn))
         (addr (string-append "127.0.0.1:" (number->string port)))
         (srv (start-http-server! addr mux: mux)))
    (thread-sleep! 0.1) ;; let server start
    (try
      (proc port)
      (finally
       (stop-http-server! srv)))))

(def handler-test
  (test-suite "handler dispatch integration"

    (test-case "simple GET returns string body"
      (let ((app (make-sinatra-app)))
        (sinatra-get app "/"
          (lambda () "Hello World"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/" port))))
              (check (request-status resp) => 200)
              (check (request-text resp) => "Hello World"))))))

    (test-case "route with named parameter"
      (let ((app (make-sinatra-app)))
        (sinatra-get app "/hello/:name"
          (lambda () (string-append "Hi " (param "name"))))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/hello/Alice" port))))
              (check (request-status resp) => 200)
              (check (request-text resp) => "Hi Alice"))))))

    (test-case "POST route"
      (let ((app (make-sinatra-app)))
        (sinatra-post app "/data"
          (lambda () (status! 201) "Created"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-post (format "http://127.0.0.1:~a/data" port)
                          data: "test")))
              (check (request-status resp) => 201)
              (check (request-text resp) => "Created"))))))

    (test-case "404 for unmatched route"
      (let ((app (make-sinatra-app)))
        (sinatra-get app "/" (lambda () "home"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/nonexistent" port))))
              (check (request-status resp) => 404))))))

    (test-case "custom 404 handler"
      (let ((app (make-sinatra-app)))
        (sinatra-not-found app (lambda () "Custom 404"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/nope" port))))
              (check (request-status resp) => 404)
              (check (request-text resp) => "Custom 404"))))))

    (test-case "JSON response from hash-table"
      (let ((app (make-sinatra-app)))
        (sinatra-get app "/api"
          (lambda () (hash ("key" "value"))))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/api" port))))
              (check (request-status resp) => 200)
              (let ((body (string->json-object (request-text resp))))
                (check (hash-ref body "key") => "value")))))))

    (test-case "before filter runs"
      (let ((app (make-sinatra-app)))
        (sinatra-before app (lambda () (header! "X-Test" "filtered")))
        (sinatra-get app "/" (lambda () "ok"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/" port))))
              (check (request-status resp) => 200)
              (check (request-text resp) => "ok"))))))

    (test-case "multiple routes first match wins"
      (let ((app (make-sinatra-app)))
        (sinatra-get app "/test" (lambda () "first"))
        (sinatra-get app "/test" (lambda () "second"))
        (with-test-server app
          (lambda (port)
            (let ((resp (http-get (format "http://127.0.0.1:~a/test" port))))
              (check (request-text resp) => "first"))))))
  ))
