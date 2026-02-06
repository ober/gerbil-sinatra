(import :std/text/json
        ./context
        ./response
        ./request
        ./mime)

(export halt
        pass
        redirect
        status!
        headers!
        header!
        content-type!
        body!
        json
        send-file
        attachment
        etag!
        last-modified!
        cache-control!
        url-for
        back
        current-error
        env
        development?
        production?
        test?)

;; Parameter: holds exception in error handler context
(def current-error (make-parameter #f))

;; Halt — immediately finish the response
(def (halt . args)
  (match args
    ([] (void))
    ([s]
     (if (integer? s)
       (status! s)
       (body! s)))
    ([s b]
     (status! s)
     (body! b))
    ([s h b]
     (status! s)
     (headers! h)
     (body! b)))
  (let ((k (current-halt-k)))
    (when k (k (void)))))

;; Pass — skip to next matching route
(def (pass)
  (let ((k (current-pass-k)))
    (when k (k #f))))

;; Redirect — send redirect response and halt
(def (redirect path (status 302))
  (status! status)
  (header! "Location" path)
  (halt))

;; Set response status
(def (status! code)
  (sinatra-response-status-set! (current-response) code))

;; Set multiple headers from alist
(def (headers! alist)
  (for-each
    (lambda (pair)
      (sinatra-response-header-set! (current-response) (car pair) (cdr pair)))
    alist))

;; Set a single header
(def (header! name value)
  (sinatra-response-header-set! (current-response) name value))

;; Set Content-Type (string or symbol shorthand)
(def (content-type! type)
  (let ((ct (if (symbol? type)
              (mime-type-sym type)
              type)))
    (sinatra-response-content-type-set! (current-response) ct)))

;; Set response body
(def (body! content)
  (sinatra-response-body-set! (current-response) content))

;; Serialize hash-table to JSON and set content-type
(def (json data)
  (content-type! "application/json")
  (json-object->string data))

;; Send a file as the response
(def (send-file path
                filename: (filename #f)
                disposition: (disposition "inline"))
  (let* ((fname (or filename (path-strip-directory path)))
         (ct (or (mime-type-for path) "application/octet-stream")))
    (content-type! ct)
    (header! "Content-Disposition"
      (string-append disposition "; filename=\"" fname "\""))
    (let ((data (call-with-input-file path
                  (lambda (port)
                    (read-all-as-string port)))))
      (body! data)
      (halt))))

;; Set Content-Disposition to attachment
(def (attachment (filename #f))
  (if filename
    (header! "Content-Disposition"
      (string-append "attachment; filename=\"" filename "\""))
    (header! "Content-Disposition" "attachment")))

;; Set ETag header
(def (etag! value)
  (header! "ETag" (string-append "\"" value "\"")))

;; Set Last-Modified header
(def (last-modified! value)
  (header! "Last-Modified" value))

;; Set Cache-Control header
(def (cache-control! value)
  (header! "Cache-Control" value))

;; Build absolute URL from path
(def (url-for path)
  (let* ((req (current-request))
         (host (sinatra-request-host req))
         (scheme (if (sinatra-request-secure? req) "https" "http")))
    (string-append scheme "://" host path)))

;; Redirect back to the referrer
(def (back)
  (redirect (or (sinatra-request-header (current-request) "Referer") "/")))

;; Environment helpers
(def (env)
  (let ((a (current-app)))
    (if a
      (let ((settings (hash-get a "settings")))
        ;; Will be properly implemented when app module is available
        "development")
      "development")))

(def (development?)
  (string=? (env) "development"))

(def (production?)
  (string=? (env) "production"))

(def (test?)
  (string=? (env) "test"))

;; Helper: read entire file as string
(def (read-all-as-string port)
  (let loop ((chars []))
    (let ((ch (read-char port)))
      (if (eof-object? ch)
        (list->string (reverse chars))
        (loop (cons ch chars))))))
