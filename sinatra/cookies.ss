(import :std/net/uri)

(export parse-cookie-header
        make-set-cookie
        request-cookies)

;; Parse "Cookie: name=value; name2=value2" into hash-table
(def (parse-cookie-header header-str)
  (let ((ht (make-hash-table test: equal?)))
    (when (and header-str (not (string=? header-str "")))
      (for-each
        (lambda (pair-str)
          (let* ((trimmed (str-trim pair-str))
                 (eq-pos (string-index trimmed #\=)))
            (when eq-pos
              (let ((name (str-trim (substring trimmed 0 eq-pos)))
                    (value (str-trim (substring trimmed (+ eq-pos 1)
                                       (string-length trimmed)))))
                (hash-put! ht name (uri-decode value))))))
        (string-split header-str #\;)))
    ht))

;; Generate a Set-Cookie header value
(def (make-set-cookie name value
                      path: (path "/")
                      domain: (domain #f)
                      max-age: (max-age #f)
                      expires: (expires #f)
                      secure: (secure #f)
                      http-only: (http-only #t)
                      same-site: (same-site "Lax"))
  (let ((parts [(string-append name "=" (uri-encode value))
                (string-append "Path=" path)]))
    (when domain
      (set! parts (append parts [(string-append "Domain=" domain)])))
    (when max-age
      (set! parts (append parts [(string-append "Max-Age=" (number->string max-age))])))
    (when expires
      (set! parts (append parts [(string-append "Expires=" expires)])))
    (when secure
      (set! parts (append parts ["Secure"])))
    (when http-only
      (set! parts (append parts ["HttpOnly"])))
    (when same-site
      (set! parts (append parts [(string-append "SameSite=" same-site)])))
    (string-join parts "; ")))

;; Get cookies from a sinatra-request (by reading raw headers)
(def (request-cookies raw-req header-fn)
  (let ((cookie-header (header-fn raw-req "Cookie")))
    (if cookie-header
      (parse-cookie-header cookie-header)
      (make-hash-table test: equal?))))

;; Helper: join strings with separator
(def (string-join strings sep)
  (if (null? strings)
    ""
    (let loop ((rest (cdr strings))
               (result (car strings)))
      (if (null? rest)
        result
        (loop (cdr rest)
              (string-append result sep (car rest)))))))

;; Helper: find index of char in string
(def (string-index str ch)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

;; Helper: trim whitespace
(def (str-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref s i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring s start end)))
