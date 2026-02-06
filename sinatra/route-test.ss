(import :std/test
        :std/pregexp
        ./route)

(export route-test)

;; Helper: compile pattern and match against path, return params or #f
(def (route-match-path pattern path)
  (let-values (((rx names) (compile-route-pattern pattern)))
    (let ((rt (make-route 'GET pattern rx names (lambda () #f) [])))
      (route-match rt 'GET path))))

;; Helper: compile pattern, match with specific method
(def (route-match-full route-method pattern req-method path)
  (let-values (((rx names) (compile-route-pattern pattern)))
    (let ((rt (make-route route-method pattern rx names (lambda () #f) [])))
      (route-match rt req-method path))))

(def route-test
  (test-suite "route pattern compilation and matching"

    (test-case "exact path match"
      (let ((p (route-match-path "/" "/")))
        (check p ? hash-table?)))

    (test-case "exact path no match"
      (check (route-match-path "/hello" "/world") => #f))

    (test-case "named parameter"
      (let ((p (route-match-path "/users/:id" "/users/42")))
        (check p ? hash-table?)
        (check (hash-ref p "id") => "42")))

    (test-case "multiple named parameters"
      (let ((p (route-match-path "/users/:uid/posts/:pid" "/users/1/posts/2")))
        (check (hash-ref p "uid") => "1")
        (check (hash-ref p "pid") => "2")))

    (test-case "splat"
      (let ((p (route-match-path "/files/*" "/files/path/to/file.txt")))
        (check p ? hash-table?)
        (check (hash-ref p "splat") => '("path/to/file.txt"))))

    (test-case "multiple splats"
      (let ((p (route-match-path "/say/*/to/*" "/say/hello/to/world")))
        (check p ? hash-table?)
        (check (hash-ref p "splat") => '("hello" "world"))))

    (test-case "optional parameter present"
      (let ((p (route-match-path "/posts/:format?" "/posts/json")))
        (check p ? hash-table?)
        (check (hash-ref p "format") => "json")))

    (test-case "optional parameter absent"
      (let ((p (route-match-path "/posts/:format?" "/posts")))
        (check p ? hash-table?)))

    (test-case "named param with dot in path"
      (let ((p (route-match-path "/users/:name" "/users/alice")))
        (check (hash-ref p "name") => "alice")))

    (test-case "method filter matches"
      (let ((p (route-match-full 'GET "/hello" 'GET "/hello")))
        (check p ? hash-table?)))

    (test-case "method filter rejects"
      (check (route-match-full 'GET "/hello" 'POST "/hello") => #f))

    (test-case "any method matches"
      (let ((p (route-match-full #f "/hello" 'DELETE "/hello")))
        (check p ? hash-table?)))

    (test-case "path with extension"
      (let ((p (route-match-path "/files/:name.:ext" "/files/report.pdf")))
        (check (hash-ref p "name") => "report")
        (check (hash-ref p "ext") => "pdf")))
  ))
