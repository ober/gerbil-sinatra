(import :std/text/json
        :std/text/base64
        :std/crypto
        ./cookies
        ./context
        ./response
        ./request)

(export session
        session-ref
        session-set!
        session-delete!
        session-clear!
        session-destroy!
        load-session
        save-session
        generate-session-secret
        current-session
        encode-session
        decode-session)

;; Parameter holding current session data
(def current-session (make-parameter #f))

;; Session cookie name
(def +session-cookie-name+ "sinatra.session")

;; Encode session data as signed cookie value
;; Format: base64(json).base64(hmac-sha256(base64(json), secret))
(def (encode-session data secret)
  (let* ((json-str (json-object->string data))
         (json-b64 (u8vector->base64-string (string->bytes json-str)))
         (sig (hmac-sha256 (string->bytes secret) (string->bytes json-b64)))
         (sig-b64 (u8vector->base64-string sig)))
    (string-append json-b64 "." sig-b64)))

;; Decode and verify a signed session cookie
;; Returns hash-table or #f if invalid/tampered
(def (decode-session cookie-val secret)
  (let ((dot-pos (string-index-of cookie-val #\.)))
    (and dot-pos
         (let* ((json-b64 (substring cookie-val 0 dot-pos))
                (sig-b64 (substring cookie-val (+ dot-pos 1)
                           (string-length cookie-val)))
                (expected-sig (hmac-sha256 (string->bytes secret)
                                           (string->bytes json-b64)))
                (expected-b64 (u8vector->base64-string expected-sig)))
           (and (string=? sig-b64 expected-b64)
                (let ((json-str (bytes->string
                                  (base64-string->u8vector json-b64))))
                  (with-catch
                    (lambda (e) #f)
                    (lambda () (string->json-object json-str)))))))))

;; Get or create the session hash-table
(def (session)
  (or (current-session)
      (let ((s (make-hash-table test: equal?)))
        (current-session s)
        s)))

;; Session accessors
(def (session-ref key)
  (hash-get (session) key))

(def (session-set! key value)
  (hash-put! (session) key value))

(def (session-delete! key)
  (hash-remove! (session) key))

(def (session-clear!)
  (current-session (make-hash-table test: equal?)))

(def (session-destroy!)
  (current-session (make-hash-table test: equal?)))

;; Load session from request cookie
(def (load-session app sreq)
  (let* ((secret (hash-get (hash-ref app 'settings) "session-secret"))
         (cookie-header (sinatra-request-header sreq "Cookie"))
         (cookies (if cookie-header
                    (parse-cookie-header cookie-header)
                    (make-hash-table test: equal?)))
         (session-cookie (hash-get cookies +session-cookie-name+)))
    (if (and session-cookie secret)
      (let ((data (decode-session session-cookie secret)))
        (current-session (or data (make-hash-table test: equal?))))
      (current-session (make-hash-table test: equal?)))))

;; Save session to response Set-Cookie
(def (save-session app sres)
  (let ((secret (hash-get (hash-ref app 'settings) "session-secret"))
        (data (current-session)))
    (when (and secret data)
      (let ((cookie-val (encode-session data secret)))
        (sinatra-response-cookie-set! sres
          (make-set-cookie +session-cookie-name+ cookie-val
            path: "/"
            http-only: #t
            same-site: "Lax"))))))

;; Generate a random session secret (64 hex chars)
(def (generate-session-secret)
  (let ((bytes (random-bytes 32)))
    (let loop ((i 0) (result ""))
      (if (>= i (u8vector-length bytes))
        result
        (loop (+ i 1)
              (string-append result
                (let ((b (u8vector-ref bytes i)))
                  (string-append
                    (number->string (quotient b 16) 16)
                    (number->string (remainder b 16) 16)))))))))

;; Helper: find first index of char in string
(def (string-index-of str ch)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))
