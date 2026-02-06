(import :std/test
        ./context)

(export context-test)

(def context-test
  (test-suite "request context parameters"

    (test-case "param returns value from current-params"
      (parameterize ((current-params (hash ("name" "Alice") ("age" "30"))))
        (check (param "name") => "Alice")
        (check (param "age") => "30")))

    (test-case "param returns #f for missing key"
      (parameterize ((current-params (hash ("name" "Alice"))))
        (check (param "missing") => #f)))

    (test-case "param! returns value when present"
      (parameterize ((current-params (hash ("name" "Alice"))))
        (check (param! "name") => "Alice")))

    (test-case "param! raises error when missing"
      (parameterize ((current-params (hash)))
        (check-exception (param! "missing") true)))

    (test-case "params returns the full hash"
      (let ((p (hash ("a" 1) ("b" 2))))
        (parameterize ((current-params p))
          (check (params) => p))))

    (test-case "splat returns empty list by default"
      (parameterize ((current-params (hash)))
        (check (splat) => [])))

    (test-case "splat returns splat values"
      (parameterize ((current-params (hash ("splat" ["foo" "bar"]))))
        (check (splat) => ["foo" "bar"])))

    (test-case "captures returns captures values"
      (parameterize ((current-params (hash ("captures" ["a" "b"]))))
        (check (captures) => ["a" "b"])))
  ))
