;; Gerbil-Sinatra: A Sinatra-style web framework for Gerbil Scheme
;;
;; Usage:
;;   (import :sinatra)
;;   (GET "/" "Hello World!")
;;   (RUN!)

(import ./sinatra/context
        ./sinatra/request
        ./sinatra/response
        ./sinatra/route
        ./sinatra/app
        ./sinatra/handler
        ./sinatra/helpers
        ./sinatra/filters
        ./sinatra/errors
        ./sinatra/cookies
        ./sinatra/session
        ./sinatra/static
        ./sinatra/template
        ./sinatra/middleware
        ./sinatra/logging
        ./sinatra/mime
        ./sinatra/dsl)

(export
  ;; DSL macros (uppercase — wrap body in lambda)
  GET POST PUT DELETE PATCH OPTIONS HEAD
  ;; DSL functions (lowercase — take explicit lambda)
  get post put delete* patch options head
  ;; Filters
  before after
  ;; Error handlers
  not-found error-handler
  ;; Configuration
  configure set-option! enable! disable!
  ;; Middleware
  use!
  ;; Server
  RUN! run!

  ;; Modular-style (explicit app)
  sinatra-get sinatra-post sinatra-put sinatra-delete
  sinatra-patch sinatra-options sinatra-head
  sinatra-before sinatra-after
  sinatra-not-found sinatra-error-handler
  sinatra-run!
  make-sinatra-app default-app

  ;; Context accessors
  param param! params request response app splat captures
  current-app current-request current-response current-params

  ;; Request accessors
  sinatra-request?
  sinatra-request-method sinatra-request-path sinatra-request-url
  sinatra-request-query-string sinatra-request-query-params
  sinatra-request-header sinatra-request-body-string
  sinatra-request-body-json sinatra-request-body-params
  sinatra-request-content-type sinatra-request-content-length
  sinatra-request-host sinatra-request-ip
  sinatra-request-xhr? sinatra-request-secure?
  sinatra-request-user-agent sinatra-request-accept

  ;; Response helpers
  halt pass redirect
  status! headers! header! content-type! body!
  json send-file attachment
  etag! last-modified! cache-control!
  url-for back
  current-error env development? production? test?

  ;; Response object
  sinatra-response-status sinatra-response-status-set!
  sinatra-response-body sinatra-response-body-set!
  sinatra-response-header sinatra-response-header-set!
  sinatra-response-content-type sinatra-response-content-type-set!

  ;; Session
  session session-ref session-set! session-delete!
  session-clear! session-destroy!

  ;; Template
  render-sxml render-file-template render-string views-path

  ;; Cookies
  parse-cookie-header make-set-cookie

  ;; MIME
  mime-type-for mime-type-for-ext mime-type-sym

  ;; Logging
  sinatra-logger current-logger log-request

  ;; App settings
  app-setting app-setting-set! app-enable! app-disable!
  app-add-route! app-add-before! app-add-after!
  app-set-not-found! app-set-error-handler!
  app-add-middleware!
  )
