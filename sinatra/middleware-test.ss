(import :std/test
        ./middleware)

(export middleware-test)

(def middleware-test
  (test-suite "middleware composition"

    (test-case "no middleware returns handler unchanged"
      (let* ((handler (lambda (req res) 'original))
             (composed (compose-middleware [] handler)))
        (check (composed 'req 'res) => 'original)))

    (test-case "single middleware wraps handler"
      (let* ((log [])
             (mw (lambda (handler)
                   (lambda (req res)
                     (set! log (append log ["before"]))
                     (handler req res)
                     (set! log (append log ["after"])))))
             (handler (lambda (req res)
                        (set! log (append log ["handler"]))))
             (composed (compose-middleware [mw] handler)))
        (composed 'req 'res)
        (check log => ["before" "handler" "after"])))

    (test-case "multiple middleware execute in correct order"
      (let* ((log [])
             (mw1 (lambda (handler)
                    (lambda (req res)
                      (set! log (append log ["mw1-before"]))
                      (handler req res)
                      (set! log (append log ["mw1-after"])))))
             (mw2 (lambda (handler)
                    (lambda (req res)
                      (set! log (append log ["mw2-before"]))
                      (handler req res)
                      (set! log (append log ["mw2-after"])))))
             (handler (lambda (req res)
                        (set! log (append log ["handler"]))))
             (composed (compose-middleware [mw1 mw2] handler)))
        (composed 'req 'res)
        ;; mw1 is outermost, mw2 is inner
        (check log => ["mw1-before" "mw2-before" "handler" "mw2-after" "mw1-after"])))

    (test-case "make-middleware creates simple wrapper"
      (let* ((called? #f)
             (mw (make-middleware (lambda (req res) (set! called? #t))))
             (handler (lambda (req res) 'ok))
             (composed (mw handler)))
        (composed 'req 'res)
        (check called? => #t)))
  ))
