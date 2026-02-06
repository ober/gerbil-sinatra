#!/usr/bin/env gxi
;;; example.ss — Gerbil-Sinatra Feature Showcase
;;;
;;; Run with: gxi example.ss
;;; Then visit: http://127.0.0.1:4567/

(import ./sinatra)

;; Configuration
(configure
  (set-option! "port" 4567)
  (enable! 'sessions)
  (set-option! "session-secret" "super-secret-key-change-in-production"))

;; Before filter — runs before every request
(before
  (header! "X-Powered-By" "Gerbil-Sinatra"))

;; Before filter — only for /admin/* routes
(before "/admin/*"
  (unless (session-ref "user")
    (redirect "/login")))

;; ---- Static pages ----

(GET "/" "Welcome to Gerbil-Sinatra!")

(GET "/hello/:name"
  (string-append "<h1>Hello, " (param "name") "!</h1>"))

;; ---- JSON API ----

(GET "/api/users"
  (json (hash ("users" [
    (hash ("id" 1) ("name" "Alice"))
    (hash ("id" 2) ("name" "Bob"))]))))

(GET "/api/users/:id"
  (let ((id (param "id")))
    (json (hash ("id" id) ("name" "User")))))

(POST "/api/users"
  (let ((body (sinatra-request-body-json (request))))
    (status! 201)
    (json body)))

;; ---- Session usage ----

(GET "/login"
  "<form method='post' action='/login'>
    <input name='user' placeholder='Username'>
    <button type='submit'>Login</button>
   </form>")

(POST "/login"
  (let ((body-params (sinatra-request-body-params (request))))
    (session-set! "user" (or (hash-get body-params "user") "anonymous"))
    (redirect "/admin")))

(GET "/admin"
  (let ((user (session-ref "user")))
    (string-append "Welcome, " (or user "unknown") "!")))

(GET "/logout"
  (session-destroy!)
  (redirect "/"))

;; ---- Splat routes ----

(GET "/files/*"
  (let ((path (car (splat))))
    (string-append "Requested file: " path)))

;; ---- Redirects ----

(GET "/old-path"
  (redirect "/new-path" 301))

(GET "/new-path"
  "You've been redirected!")

;; ---- Templates ----

(GET "/template"
  (render-string "<h1>{{title}}</h1><p>{{message}}</p>"
    (hash ("title" "Gerbil-Sinatra")
          ("message" "Templates work!"))))

(GET "/sxml"
  (render-sxml
    '(html
       (head (title "SXML Demo"))
       (body
         (h1 "SXML Rendering")
         (p "This page was generated from SXML.")
         (ul
           (li "Item 1")
           (li "Item 2")
           (li "Item 3"))))))

;; ---- Error handlers ----

(not-found
  "<h1>404</h1><p>Nothing here!</p>")

(error-handler
  (string-append "<h1>500</h1><p>Something went wrong</p>"))

;; ---- Start the server ----

(displayln "Starting Gerbil-Sinatra example...")
(displayln "Visit http://127.0.0.1:4567/")
(let ((srv (RUN!)))
  ;; Keep main thread alive
  (thread-join! srv))
