(export current-app
        current-request
        current-response
        current-params
        current-halt-k
        current-pass-k
        current-raw-response
        param
        param!
        params
        request
        response
        app
        splat
        captures)

;; Dynamic parameters for request context
(def current-app (make-parameter #f))
(def current-request (make-parameter #f))
(def current-response (make-parameter #f))
(def current-params (make-parameter (hash)))
(def current-halt-k (make-parameter #f))
(def current-pass-k (make-parameter #f))
(def current-raw-response (make-parameter #f))

;; Convenience accessors for use inside route handlers

(def (param name)
  (hash-get (current-params) name))

(def (param! name)
  (or (hash-get (current-params) name)
      (error "Missing required parameter" name)))

(def (params)
  (current-params))

(def (request)
  (current-request))

(def (response)
  (current-response))

(def (app)
  (current-app))

(def (splat)
  (or (hash-get (current-params) "splat") []))

(def (captures)
  (or (hash-get (current-params) "captures") []))
