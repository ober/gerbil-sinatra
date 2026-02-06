(import :std/net/httpd
        :std/net/uri
        :std/text/json)

(export make-sinatra-request sinatra-request?
        wrap-request
        sinatra-request-method
        sinatra-request-path
        sinatra-request-url
        sinatra-request-query-string
        sinatra-request-query-params
        sinatra-request-header
        sinatra-request-body-string
        sinatra-request-body-json
        sinatra-request-body-params
        sinatra-request-content-type
        sinatra-request-content-length
        sinatra-request-host
        sinatra-request-ip
        sinatra-request-xhr?
        sinatra-request-secure?
        sinatra-request-user-agent
        sinatra-request-accept
        sinatra-request-raw)

;; Internal struct — field names chosen to avoid clashing with public API functions
(defstruct sinatra-request
  (raw               ;; the httpd http-request object
   method            ;; cached symbol ('GET, 'POST, etc.)
   path              ;; cached path string
   query-string      ;; cached query string or #f
   hdrs              ;; cached headers alist or #f
   qparams           ;; lazy: parsed query params hash-table or #f
   body-str          ;; lazy: body as string or #f
   body-prms)        ;; lazy: body form params hash-table or #f
  transparent: #t)

;; Parse the path and query string from a URL
(def (split-path-query url)
  (let ((qpos (let loop ((i 0))
                (cond
                  ((>= i (string-length url)) #f)
                  ((char=? (string-ref url i) #\?) i)
                  (else (loop (+ i 1)))))))
    (if qpos
      (values (substring url 0 qpos)
              (substring url (+ qpos 1) (string-length url)))
      (values url #f))))

;; Parse query/form string into hash-table
(def (parse-query-string qs)
  (if (or (not qs) (string=? qs ""))
    (make-hash-table test: equal?)
    (let ((ht (make-hash-table test: equal?))
          (pairs (form-url-decode qs)))
      (when (list? pairs)
        (for-each
          (lambda (pair)
            (when (pair? pair)
              (hash-put! ht
                (let ((k (car pair)))
                  (if (symbol? k) (symbol->string k) k))
                (let ((v (cdr pair)))
                  (if (symbol? v) (symbol->string v) v)))))
          pairs))
      ht)))

;; Helper: trim whitespace from string
(def (str-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref s i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring s start end)))

;; Wrap an httpd http-request into a sinatra-request
(def (wrap-request raw-req)
  (let* ((raw-method (http-request-method raw-req))
         (method (if (symbol? raw-method) raw-method (string->symbol raw-method)))
         (url (http-request-url raw-req)))
    (let-values (((path qs) (split-path-query url)))
      (make-sinatra-request
        raw-req method path qs
        #f #f #f #f))))

;; Get URL (full path + query)
(def (sinatra-request-url req)
  (http-request-url (sinatra-request-raw req)))

;; Get a single header by name (case-insensitive)
(def (sinatra-request-header req name)
  (let ((headers (http-request-headers (sinatra-request-raw req)))
        (name-down (string-downcase name)))
    (let loop ((hdrs headers))
      (cond
        ((null? hdrs) #f)
        ((pair? (car hdrs))
         (if (string=? (string-downcase (car (car hdrs))) name-down)
           (cdr (car hdrs))
           (loop (cdr hdrs))))
        (else (loop (cdr hdrs)))))))

;; Get query params as hash-table (lazy-cached)
(def (sinatra-request-query-params req)
  (or (sinatra-request-qparams req)
      (let ((params (parse-query-string (sinatra-request-query-string req))))
        (sinatra-request-qparams-set! req params)
        params)))

;; Get raw body as string (lazy-cached)
(def (sinatra-request-body-string req)
  (or (sinatra-request-body-str req)
      (let* ((body-data (http-request-body (sinatra-request-raw req)))
             (body-string (cond
                            ((string? body-data) body-data)
                            ((u8vector? body-data) (bytes->string body-data))
                            (else ""))))
        (sinatra-request-body-str-set! req body-string)
        body-string)))

;; Parse body as JSON
(def (sinatra-request-body-json req)
  (let ((body (sinatra-request-body-string req)))
    (if (or (not body) (string=? body ""))
      (make-hash-table test: equal?)
      (string->json-object body))))

;; Parse body as form-urlencoded params
(def (sinatra-request-body-params req)
  (or (sinatra-request-body-prms req)
      (let ((params (parse-query-string (sinatra-request-body-string req))))
        (sinatra-request-body-prms-set! req params)
        params)))

;; Content-Type header
(def (sinatra-request-content-type req)
  (sinatra-request-header req "Content-Type"))

;; Content-Length header
(def (sinatra-request-content-length req)
  (let ((val (sinatra-request-header req "Content-Length")))
    (and val (string->number val))))

;; Host header
(def (sinatra-request-host req)
  (or (sinatra-request-header req "Host") "localhost"))

;; Client IP
(def (sinatra-request-ip req)
  (let ((client (http-request-client (sinatra-request-raw req))))
    (if (string? client) client
        (if (pair? client) (car client) "127.0.0.1"))))

;; XHR request?
(def (sinatra-request-xhr? req)
  (let ((xrw (sinatra-request-header req "X-Requested-With")))
    (and xrw (string=? (string-downcase xrw) "xmlhttprequest"))))

;; Secure connection?
(def (sinatra-request-secure? req)
  (let ((proto (http-request-proto (sinatra-request-raw req))))
    (and (string? proto) (string=? proto "HTTPS"))))

;; User-Agent header
(def (sinatra-request-user-agent req)
  (sinatra-request-header req "User-Agent"))

;; Accept header parsed into list of MIME types
(def (sinatra-request-accept req)
  (let ((accept (sinatra-request-header req "Accept")))
    (if accept
      (map str-trim (string-split accept #\,))
      [])))
