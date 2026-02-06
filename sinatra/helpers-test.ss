(import :std/test
        :std/text/json
        ./context
        ./response
        ./helpers)

(export helpers-test)

(def helpers-test
  (test-suite "helper functions"

    (test-case "status! sets response status"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (status! 404)
          (check (sinatra-response-status sres) => 404))))

    (test-case "header! sets response header"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (header! "X-Custom" "value")
          (check (sinatra-response-header sres "X-Custom") => "value"))))

    (test-case "headers! sets multiple headers"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (headers! (list (cons "X-One" "1") (cons "X-Two" "2")))
          (check (sinatra-response-header sres "X-One") => "1")
          (check (sinatra-response-header sres "X-Two") => "2"))))

    (test-case "content-type! sets Content-Type"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (content-type! "application/json")
          (check (sinatra-response-content-type sres) => "application/json"))))

    (test-case "content-type! with symbol"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (content-type! 'json)
          (check (sinatra-response-content-type sres) => "application/json"))))

    (test-case "body! sets response body"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (body! "hello")
          (check (sinatra-response-body sres) => "hello"))))

    (test-case "json serializes and sets content-type"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (let ((result (json (hash ("key" "value")))))
            (check result ? string?)
            (check (sinatra-response-content-type sres) => "application/json")
            (let ((parsed (string->json-object result)))
              (check (hash-ref parsed "key") => "value"))))))

    (test-case "etag! sets ETag header"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (etag! "abc123")
          (check (sinatra-response-header sres "ETag") => "\"abc123\""))))

    (test-case "cache-control! sets Cache-Control header"
      (let ((sres (new-sinatra-response)))
        (parameterize ((current-response sres))
          (cache-control! "public, max-age=3600")
          (check (sinatra-response-header sres "Cache-Control")
                 => "public, max-age=3600"))))

    (test-case "development? returns true by default"
      (check (development?) => #t))
  ))
