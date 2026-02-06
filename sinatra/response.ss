(import :std/net/httpd)

(export new-sinatra-response sinatra-response?
        sinatra-response-status sinatra-response-status-set!
        sinatra-response-body sinatra-response-body-set!
        sinatra-response-halted? sinatra-response-halted?-set!
        sinatra-response-header
        sinatra-response-header-set!
        sinatra-response-headers-alist
        sinatra-response-content-type
        sinatra-response-content-type-set!
        sinatra-response-cookie-set!
        sinatra-response-redirect!
        sinatra-response-write!)

(defstruct sinatra-response
  (status           ;; integer, default 200
   hdrs             ;; hash-table of header name -> value
   body             ;; string or #f
   cookies          ;; list of Set-Cookie strings
   halted?)         ;; boolean
  transparent: #t)

;; Constructor with defaults (named differently to avoid conflict with defstruct)
(def (new-sinatra-response)
  (make-sinatra-response 200 (make-hash-table test: equal?) #f [] #f))

;; Get a single response header
(def (sinatra-response-header res name)
  (hash-get (sinatra-response-hdrs res) name))

;; Set a single response header
(def (sinatra-response-header-set! res name value)
  (hash-put! (sinatra-response-hdrs res) name value))

;; Get all headers as alist
(def (sinatra-response-headers-alist res)
  (let ((alist []))
    (hash-for-each
      (lambda (k v) (set! alist (cons (cons k v) alist)))
      (sinatra-response-hdrs res))
    ;; Append Set-Cookie headers
    (for-each
      (lambda (cookie)
        (set! alist (cons (cons "Set-Cookie" cookie) alist)))
      (sinatra-response-cookies res))
    alist))

;; Get Content-Type
(def (sinatra-response-content-type res)
  (hash-get (sinatra-response-hdrs res) "Content-Type"))

;; Set Content-Type
(def (sinatra-response-content-type-set! res ct)
  (hash-put! (sinatra-response-hdrs res) "Content-Type" ct))

;; Queue a Set-Cookie header
(def (sinatra-response-cookie-set! res cookie-str)
  (sinatra-response-cookies-set! res
    (cons cookie-str (sinatra-response-cookies res))))

;; Set redirect
(def (sinatra-response-redirect! res location (status 302))
  (sinatra-response-status-set! res status)
  (sinatra-response-header-set! res "Location" location))

;; Write the accumulated response to the httpd response object.
;; httpd-res is the raw http-response from the handler arguments.
;; http-response-write takes (res status headers body) where body is string or #f.
(def (sinatra-response-write! sres httpd-res)
  (let* ((status (sinatra-response-status sres))
         (headers-alist (sinatra-response-headers-alist sres))
         (body (sinatra-response-body sres))
         (body-str (cond
                     ((string? body) body)
                     ((u8vector? body) (bytes->string body))
                     (else #f))))
    ;; Ensure Content-Type is set
    (unless (sinatra-response-content-type sres)
      (set! headers-alist
        (cons (cons "Content-Type" "text/html; charset=utf-8") headers-alist)))
    ;; Write via httpd: (http-response-write res status headers body)
    (http-response-write httpd-res status headers-alist body-str)))
