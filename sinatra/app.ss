(import ./route
        ./context)

(export make-sinatra-app
        app-add-route!
        app-add-before!
        app-add-after!
        app-set-not-found!
        app-set-error-handler!
        app-setting
        app-setting-set!
        app-enable!
        app-disable!
        app-add-middleware!
        app-routes
        app-before-filters
        app-after-filters
        app-middleware
        app-not-found-handler
        app-error-handlers
        app-environment
        default-app
        default-settings)

;; Default settings hash
(def default-settings
  (hash
   ("port" 4567)
   ("bind" "127.0.0.1")
   ("public-folder" "./public")
   ("views" "./views")
   ("sessions" #f)
   ("session-secret" #f)
   ("static" #t)
   ("logging" #t)
   ("show-exceptions" #t)
   ("dump-errors" #t)
   ("method-override" #f)
   ("default-content-type" "text/html; charset=utf-8")
   ("environment" "development")))

;; App is a hash-table holding all registrations.
;; Using a hash-table rather than defclass to keep things simple
;; and avoid circular dependency issues.
(def (make-sinatra-app)
  (let ((app (make-hash-table test: equal?)))
    (hash-put! app 'routes [])
    (hash-put! app 'before-filters [])
    (hash-put! app 'after-filters [])
    (hash-put! app 'error-handlers (make-hash-table))
    (hash-put! app 'not-found-handler #f)
    (hash-put! app 'settings (hash-copy default-settings))
    (hash-put! app 'middleware [])
    (hash-put! app 'environment "development")
    app))

;; Global default app for classic mode
(def default-app (make-sinatra-app))

;; Register a route
(def (app-add-route! app method pattern handler (conditions []))
  (let-values (((rx names) (compile-route-pattern pattern)))
    (let ((rt (make-route method pattern rx names handler conditions)))
      (hash-put! app 'routes
        (append (hash-ref app 'routes) [rt])))))

;; Register a before filter
(def (app-add-before! app pattern handler)
  (hash-put! app 'before-filters
    (append (hash-ref app 'before-filters)
            [(cons pattern handler)])))

;; Register an after filter
(def (app-add-after! app pattern handler)
  (hash-put! app 'after-filters
    (append (hash-ref app 'after-filters)
            [(cons pattern handler)])))

;; Set the 404 handler
(def (app-set-not-found! app handler)
  (hash-put! app 'not-found-handler handler))

;; Set an error handler (keyed by symbol or status code)
(def (app-set-error-handler! app key handler)
  (let ((handlers (hash-ref app 'error-handlers)))
    (hash-put! handlers key handler)))

;; Get a setting value
(def (app-setting app key)
  (let ((settings (hash-ref app 'settings)))
    (hash-get settings key)))

;; Set a setting value
(def (app-setting-set! app key value)
  (let ((settings (hash-ref app 'settings)))
    (hash-put! settings key value)))

;; Enable a setting (set to #t)
(def (app-enable! app key)
  (app-setting-set! app (if (symbol? key) (symbol->string key) key) #t))

;; Disable a setting (set to #f)
(def (app-disable! app key)
  (app-setting-set! app (if (symbol? key) (symbol->string key) key) #f))

;; Add middleware
(def (app-add-middleware! app mw)
  (hash-put! app 'middleware
    (append (hash-ref app 'middleware) [mw])))

;; Accessors
(def (app-routes app) (hash-ref app 'routes))
(def (app-before-filters app) (hash-ref app 'before-filters))
(def (app-after-filters app) (hash-ref app 'after-filters))
(def (app-middleware app) (hash-ref app 'middleware))
(def (app-not-found-handler app) (hash-get app 'not-found-handler))
(def (app-error-handlers app) (hash-ref app 'error-handlers))

(def (app-environment app)
  (or (app-setting app "environment") "development"))
