(import :std/test
        :std/crypto
        ./session
        ./context)

(export session-test)

(def session-test
  (test-suite "session encode/decode/sign"

    (test-case "session returns hash-table"
      (parameterize ((current-session #f))
        (check (session) ? hash-table?)))

    (test-case "session-set! and session-ref"
      (parameterize ((current-session #f))
        (session-set! "user" "Alice")
        (check (session-ref "user") => "Alice")))

    (test-case "session-delete!"
      (parameterize ((current-session #f))
        (session-set! "key" "val")
        (session-delete! "key")
        (check (session-ref "key") => #f)))

    (test-case "session-clear! removes all data"
      (parameterize ((current-session #f))
        (session-set! "a" 1)
        (session-set! "b" 2)
        (session-clear!)
        (check (session-ref "a") => #f)
        (check (session-ref "b") => #f)))

    (test-case "encode then decode returns original data"
      (let* ((data (hash ("user" "Alice") ("role" "admin")))
             (secret "test-secret-key")
             (encoded (encode-session data secret))
             (decoded (decode-session encoded secret)))
        (check decoded ? hash-table?)
        (check (hash-ref decoded "user") => "Alice")
        (check (hash-ref decoded "role") => "admin")))

    (test-case "decode with wrong secret returns #f"
      (let* ((data (hash ("user" "Alice")))
             (encoded (encode-session data "correct-secret"))
             (decoded (decode-session encoded "wrong-secret")))
        (check decoded => #f)))

    (test-case "decode tampered data returns #f"
      (let* ((data (hash ("user" "Alice")))
             (encoded (encode-session data "secret"))
             (tampered (string-append "tampered." (substring encoded
                         (+ 1 (let loop ((i 0))
                                 (if (char=? (string-ref encoded i) #\.)
                                   i (loop (+ i 1)))))
                         (string-length encoded)))))
        (check (decode-session tampered "secret") => #f)))

    (test-case "generate-session-secret returns 64-char hex string"
      (let ((secret (generate-session-secret)))
        (check secret ? string?)
        (check (string-length secret) => 64)))
  ))
