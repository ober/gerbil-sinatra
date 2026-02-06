(export compose-middleware
        make-middleware)

;; Compose a list of middleware around a handler.
;; Each middleware is (lambda (handler) (lambda (req res) ...))
;; Outermost middleware is first in the list.
(def (compose-middleware middleware-list handler)
  (if (null? middleware-list)
    handler
    (let loop ((mws (reverse middleware-list))
               (h handler))
      (if (null? mws)
        h
        (loop (cdr mws) ((car mws) h))))))

;; Helper to create a simple middleware from a before-proc.
;; before-proc is called before the handler with (req res).
(def (make-middleware before-proc)
  (lambda (handler)
    (lambda (req res)
      (before-proc req res)
      (handler req res))))
