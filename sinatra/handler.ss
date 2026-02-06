(import :std/text/json
        :std/sugar
        ./context
        ./request
        ./response
        ./route
        ./app
        ./filters
        ./errors
        ./helpers
        ./static
        ./session
        ./middleware)

(export sinatra-handler)

;; Create an httpd handler function for the given sinatra app.
;; Returns (lambda (req res) ...) suitable for httpd mux.
(def (sinatra-handler app)
  (let ((base-handler (make-base-handler app))
        (mw-list (app-middleware app)))
    (compose-middleware mw-list base-handler)))

;; The core handler logic
(def (make-base-handler app)
  (lambda (req res)
    (let* ((sreq (wrap-request req))
           (sres (new-sinatra-response))
           (method (sinatra-request-method sreq))
           (path (sinatra-request-path sreq)))
      ;; Set up context parameters
      (parameterize ((current-app app)
                     (current-request sreq)
                     (current-response sres)
                     (current-params (sinatra-request-query-params sreq))
                     (current-raw-response res))
        ;; Halt continuation: calling (halt) jumps here
        (let/cc halt-k
          (parameterize ((current-halt-k halt-k))
            (try
              ;; Handle method override if enabled
              (let ((effective-method
                     (if (and (eq? method 'POST)
                              (app-setting app "method-override"))
                       (let ((override (or (hash-get (sinatra-request-query-params sreq) "_method")
                                           #f)))
                         (if override
                           (string->symbol (string-upcase override))
                           method))
                       method)))

                ;; 1. Check static files first
                (when (and (eq? effective-method 'GET)
                           (app-setting app "static"))
                  (let ((static-path (try-static-file app path)))
                    (when static-path
                      (serve-static-file static-path res)
                      (halt-k (void)))))

                ;; 2. Load session if enabled
                (when (app-setting app "sessions")
                  (load-session app sreq))

                ;; 3. Run before filters
                (run-before-filters app effective-method path)

                ;; 4. Try routes
                (let ((result (try-routes app effective-method path)))
                  (if result
                    (interpret-result result sres)
                    (handle-not-found app sres)))

                ;; 5. Run after filters
                (run-after-filters app effective-method path)

                ;; 6. Save session if enabled
                (when (app-setting app "sessions")
                  (save-session app sres)))

              (catch (e)
                (handle-error app sres e)))))

        ;; Write final response (unless already sent, e.g. static file)
        (unless (sinatra-response-halted? sres)
          (sinatra-response-write! sres res))))))

;; Try all routes in order, return the first matching handler's result or #f.
(def (try-routes app method path)
  (let loop ((routes (app-routes app)))
    (match routes
      ([] #f)
      ([rt . rest]
       (let ((route-params (route-match rt method path)))
         (if route-params
           ;; Merge route params with query params
           (let ((merged (hash-merge (current-params) route-params)))
             (parameterize ((current-params merged))
               ;; pass continuation: skip to next route
               (let/cc pass-k
                 (parameterize ((current-pass-k pass-k))
                   (let ((result ((route-handler rt))))
                     ;; If pass was called, pass-k returns #f, and we continue
                     ;; If handler returned normally, return the result
                     result)))))
           (loop rest)))))))

;; Interpret the return value of a route handler and apply to response.
(def (interpret-result result sres)
  (cond
    ((string? result)
     (sinatra-response-body-set! sres result))
    ((integer? result)
     (sinatra-response-status-set! sres result))
    ((hash-table? result)
     (sinatra-response-content-type-set! sres "application/json")
     (sinatra-response-body-set! sres (json-object->string result)))
    ((pair? result)
     (interpret-list-result result sres))
    ((void? result) (void))
    ((u8vector? result)
     (sinatra-response-body-set! sres (bytes->string result)))))

;; Interpret a list result: [status body] or [status headers body]
(def (interpret-list-result result sres)
  (let ((len (length result)))
    (cond
      ((= len 2)
       (sinatra-response-status-set! sres (car result))
       (sinatra-response-body-set! sres (cadr result)))
      ((= len 3)
       (sinatra-response-status-set! sres (car result))
       (for-each
         (lambda (hdr)
           (when (pair? hdr)
             (sinatra-response-header-set! sres (car hdr) (cdr hdr))))
         (cadr result))
       (sinatra-response-body-set! sres (caddr result)))
      (else (void)))))

;; Merge two hash tables (second overrides first)
(def (hash-merge ht1 ht2)
  (let ((result (make-hash-table test: equal?)))
    (hash-for-each (lambda (k v) (hash-put! result k v)) ht1)
    (hash-for-each (lambda (k v) (hash-put! result k v)) ht2)
    result))
