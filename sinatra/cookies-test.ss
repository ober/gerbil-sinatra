(import :std/test
        ./cookies)

(export cookies-test)

(def cookies-test
  (test-suite "cookie parsing and generation"

    (test-case "parse simple cookie header"
      (let ((cookies (parse-cookie-header "name=Alice; age=30")))
        (check (hash-ref cookies "name") => "Alice")
        (check (hash-ref cookies "age") => "30")))

    (test-case "parse single cookie"
      (let ((cookies (parse-cookie-header "session=abc123")))
        (check (hash-ref cookies "session") => "abc123")))

    (test-case "parse empty string"
      (let ((cookies (parse-cookie-header "")))
        (check (hash->list cookies) => [])))

    (test-case "parse #f"
      (let ((cookies (parse-cookie-header #f)))
        (check (hash->list cookies) => [])))

    (test-case "make-set-cookie basic"
      (let ((cookie (make-set-cookie "name" "Alice")))
        (check cookie ? string?)
        ;; Should contain name=Alice
        (check (string-contains cookie "name=Alice") ? values)))

    (test-case "make-set-cookie with path"
      (let ((cookie (make-set-cookie "sid" "xyz" path: "/api")))
        (check (string-contains cookie "Path=/api") ? values)))

    (test-case "make-set-cookie with max-age"
      (let ((cookie (make-set-cookie "sid" "xyz" max-age: 3600)))
        (check (string-contains cookie "Max-Age=3600") ? values)))

    (test-case "make-set-cookie with secure"
      (let ((cookie (make-set-cookie "sid" "xyz" secure: #t)))
        (check (string-contains cookie "Secure") ? values)))

    (test-case "make-set-cookie with httponly"
      (let ((cookie (make-set-cookie "sid" "xyz" http-only: #t)))
        (check (string-contains cookie "HttpOnly") ? values)))
  ))

;; Helper: check if string contains substring
(def (string-contains haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))
