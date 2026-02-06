(import :std/net/httpd
        :std/format
        ./app
        ./handler
        ./context
        ./helpers)

(export GET POST PUT DELETE PATCH OPTIONS HEAD
        get post put delete* patch options head
        before after
        not-found error-handler
        configure
        set-option! enable! disable!
        use!
        RUN! run!
        sinatra-get sinatra-post sinatra-put sinatra-delete
        sinatra-patch sinatra-options sinatra-head
        sinatra-before sinatra-after
        sinatra-not-found sinatra-error-handler
        sinatra-run!)

;; ============================================================
;; Uppercase route macros — wrap body in lambda
;; ============================================================

(defrules GET ()
  ((_ pattern body ...)
   (app-add-route! default-app 'GET pattern
     (lambda () body ...))))

(defrules POST ()
  ((_ pattern body ...)
   (app-add-route! default-app 'POST pattern
     (lambda () body ...))))

(defrules PUT ()
  ((_ pattern body ...)
   (app-add-route! default-app 'PUT pattern
     (lambda () body ...))))

(defrules DELETE ()
  ((_ pattern body ...)
   (app-add-route! default-app 'DELETE pattern
     (lambda () body ...))))

(defrules PATCH ()
  ((_ pattern body ...)
   (app-add-route! default-app 'PATCH pattern
     (lambda () body ...))))

(defrules OPTIONS ()
  ((_ pattern body ...)
   (app-add-route! default-app 'OPTIONS pattern
     (lambda () body ...))))

(defrules HEAD ()
  ((_ pattern body ...)
   (app-add-route! default-app 'HEAD pattern
     (lambda () body ...))))

;; ============================================================
;; Lowercase route functions — take explicit lambda handler
;; ============================================================

(def (get pattern handler)
  (app-add-route! default-app 'GET pattern handler))

(def (post pattern handler)
  (app-add-route! default-app 'POST pattern handler))

(def (put pattern handler)
  (app-add-route! default-app 'PUT pattern handler))

(def (delete* pattern handler)
  (app-add-route! default-app 'DELETE pattern handler))

(def (patch pattern handler)
  (app-add-route! default-app 'PATCH pattern handler))

(def (options pattern handler)
  (app-add-route! default-app 'OPTIONS pattern handler))

(def (head pattern handler)
  (app-add-route! default-app 'HEAD pattern handler))

;; ============================================================
;; Filters
;; ============================================================

(defrules before ()
  ((_ pattern body0 body ...)
   (app-add-before! default-app pattern (lambda () body0 body ...)))
  ((_ body ...)
   (app-add-before! default-app #f (lambda () body ...))))

(defrules after ()
  ((_ pattern body0 body ...)
   (app-add-after! default-app pattern (lambda () body0 body ...)))
  ((_ body ...)
   (app-add-after! default-app #f (lambda () body ...))))

;; ============================================================
;; Error handlers
;; ============================================================

(defrules not-found ()
  ((_ body ...)
   (app-set-not-found! default-app (lambda () body ...))))

(defrules error-handler ()
  ((_ body ...)
   (app-set-error-handler! default-app 'error (lambda () body ...))))

;; ============================================================
;; Configuration
;; ============================================================

(defrules configure ()
  ((_ body ...)
   (begin body ...)))

(def (set-option! key value)
  (app-setting-set! default-app key value))

(def (enable! key)
  (app-enable! default-app key))

(def (disable! key)
  (app-disable! default-app key))

;; ============================================================
;; Middleware
;; ============================================================

(def (use! mw)
  (app-add-middleware! default-app mw))

;; ============================================================
;; Server start
;; ============================================================

(defrules RUN! ()
  ((_) (run!)))

(def (run! (the-app default-app)
           port: (port #f)
           bind: (bind #f))
  (let ((port (or port (app-setting the-app "port") 4567))
        (bind (or bind (app-setting the-app "bind") "127.0.0.1")))
    (let* ((handler-fn (sinatra-handler the-app))
           (mux (make-default-http-mux handler-fn))
           (addr (string-append bind ":" (number->string port)))
           (srv (start-http-server! addr mux: mux)))
      (displayln (format "== Sinatra has taken the stage on port ~a ==" port))
      srv)))

;; ============================================================
;; Modular-style (explicit app) functions
;; ============================================================

(def (sinatra-get app pattern handler)
  (app-add-route! app 'GET pattern handler))

(def (sinatra-post app pattern handler)
  (app-add-route! app 'POST pattern handler))

(def (sinatra-put app pattern handler)
  (app-add-route! app 'PUT pattern handler))

(def (sinatra-delete app pattern handler)
  (app-add-route! app 'DELETE pattern handler))

(def (sinatra-patch app pattern handler)
  (app-add-route! app 'PATCH pattern handler))

(def (sinatra-options app pattern handler)
  (app-add-route! app 'OPTIONS pattern handler))

(def (sinatra-head app pattern handler)
  (app-add-route! app 'HEAD pattern handler))

(def (sinatra-before app pattern-or-handler . rest)
  (if (null? rest)
    ;; (sinatra-before app handler) — no pattern
    (app-add-before! app #f pattern-or-handler)
    ;; (sinatra-before app pattern handler)
    (app-add-before! app pattern-or-handler (car rest))))

(def (sinatra-after app pattern-or-handler . rest)
  (if (null? rest)
    (app-add-after! app #f pattern-or-handler)
    (app-add-after! app pattern-or-handler (car rest))))

(def (sinatra-not-found app handler)
  (app-set-not-found! app handler))

(def (sinatra-error-handler app handler)
  (app-set-error-handler! app 'error handler))

(def (sinatra-run! app port: (port #f) bind: (bind #f))
  (run! app port: port bind: bind))
