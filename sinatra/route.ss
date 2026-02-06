(import :std/pregexp
        :std/iter
        :std/sugar)

(export make-route route? route-method route-pattern-src
        route-pattern-rx route-param-names route-handler route-conditions
        compile-route-pattern
        route-match)

(defstruct route
  (method            ;; symbol: GET POST PUT DELETE PATCH OPTIONS HEAD or #f (any)
   pattern-src       ;; original pattern string
   pattern-rx        ;; compiled pregexp
   param-names       ;; list of param name strings (named params) or 'splat / 'capture markers
   handler           ;; (lambda () body) — route handler
   conditions)       ;; alist of conditions
  transparent: #t)

;; Compile a Sinatra-style pattern string into a pregexp + param-names list.
;; Returns (values compiled-regex param-names-list)
;;
;; Pattern syntax:
;;   :name   -> named capture ([^/]+)
;;   *       -> splat capture (.+?)
;;   :name?  -> optional named capture (?:/([^/]+))?
;;   literal -> escaped literal
(def (compile-route-pattern pattern)
  (cond
    ;; Already a compiled regex (pregexp)
    ((pair? pattern)
     (values pattern []))
    ;; String pattern — compile it
    ((string? pattern)
     (compile-string-pattern pattern))
    (else
     (error "Invalid route pattern" pattern))))

(def (compile-string-pattern pattern)
  (let ((len (string-length pattern))
        (rx (open-output-string))
        (names []))
    ;; Write anchor
    (write-string "^" rx)
    (let loop ((i 0) (prev-slash? #f))
      (if (>= i len)
        ;; Done — close anchor and compile
        (begin
          (write-string "$" rx)
          (values (pregexp (get-output-string rx))
                  (reverse names)))
        (let ((ch (string-ref pattern i)))
          (cond
            ;; Named parameter :name or :name?
            ((char=? ch #\:)
             (let-values (((name end optional?) (read-param-name pattern (+ i 1) len)))
               (if optional?
                 (begin
                   ;; For optional params, wrap the preceding / and capture in (?:...)?
                   ;; The preceding / was already written if prev-slash? is true,
                   ;; but we need to undo it. Instead, use a regex trick:
                   ;; We already wrote the slash, so replace with optional group
                   ;; by getting current string, removing trailing /, and rewriting
                   (let* ((current (get-output-string rx)))
                     ;; Reset rx and write the modified pattern
                     (set! rx (open-output-string))
                     (if (and (> (string-length current) 0)
                              (char=? (string-ref current (- (string-length current) 1)) #\/))
                       ;; Remove trailing / and make it part of optional group
                       (begin
                         (write-string (substring current 0 (- (string-length current) 1)) rx)
                         (write-string "(?:/([^/]+))?" rx))
                       ;; No preceding slash, just make capture optional
                       (begin
                         (write-string current rx)
                         (write-string "([^/]+)?" rx))))
                   (set! names (cons name names))
                   (loop end #f))
                 (begin
                   (write-string "([^/]+)" rx)
                   (set! names (cons name names))
                   (loop end #f)))))
            ;; Splat *
            ((char=? ch #\*)
             (write-string "(.+?)" rx)
             (set! names (cons 'splat names))
             (loop (+ i 1) #f))
            ;; Escape special regex characters
            ((regex-special-char? ch)
             (write-char #\\ rx)
             (write-char ch rx)
             (loop (+ i 1) #f))
            ;; Forward slash
            ((char=? ch #\/)
             (write-char ch rx)
             (loop (+ i 1) #t))
            ;; Normal character
            (else
             (write-char ch rx)
             (loop (+ i 1) #f))))))))

;; Check if a character is a regex special character that needs escaping
(def (regex-special-char? ch)
  (let ((c (char->integer ch)))
    (or (char=? ch #\.)
        (char=? ch #\+)
        (char=? ch #\^)
        (char=? ch #\$)
        (char=? ch #\|)
        (= c 40)   ;; (
        (= c 41)   ;; )
        (= c 91)   ;; [
        (= c 93)   ;; ]
        (= c 123)  ;; {
        (= c 125)  ;; }
        (char=? ch #\\))))

;; Read a parameter name starting at position i.
;; Returns (values name-string end-position optional?)
(def (read-param-name pattern i len)
  (let loop ((j i))
    (if (or (>= j len)
            (not (or (char-alphabetic? (string-ref pattern j))
                     (char-numeric? (string-ref pattern j))
                     (char=? (string-ref pattern j) #\_)
                     (char=? (string-ref pattern j) #\-))))
      ;; Check if followed by ? (optional)
      (let ((name (substring pattern i j)))
        (if (and (< j len) (char=? (string-ref pattern j) #\?))
          (values name (+ j 1) #t)
          (values name j #f)))
      (loop (+ j 1)))))

;; Match a route against method + path.
;; Returns #f or a hash-table of params.
(def (route-match rt method path)
  (and (or (not (route-method rt))
           (eq? method (route-method rt)))
       (let ((m (pregexp-match (route-pattern-rx rt) path)))
         (and m
              (build-params (cdr m) (route-param-names rt))))))

;; Build a params hash-table from match captures and param name descriptors.
(def (build-params captures names)
  (let ((ht (make-hash-table test: equal?))
        (splats [])
        (cap-list []))
    (let loop ((captures captures) (names names))
      (cond
        ((and (null? captures) (null? names))
         ;; Store accumulated splats and captures
         (unless (null? splats)
           (hash-put! ht "splat" (reverse splats)))
         (unless (null? cap-list)
           (hash-put! ht "captures" (reverse cap-list)))
         ht)
        ((or (null? captures) (null? names))
         ;; Positional captures without names -> store as "captures"
         (for-each (lambda (c) (when c (set! cap-list (cons c cap-list)))) captures)
         (unless (null? (append cap-list))
           (hash-put! ht "captures" (reverse cap-list)))
         (unless (null? splats)
           (hash-put! ht "splat" (reverse splats)))
         ht)
        (else
         (let ((cap (car captures))
               (name (car names)))
           (cond
             ((eq? name 'splat)
              (when cap (set! splats (cons cap splats)))
              (loop (cdr captures) (cdr names)))
             ((string? name)
              (when cap (hash-put! ht name cap))
              (loop (cdr captures) (cdr names)))
             (else
              (loop (cdr captures) (cdr names))))))))))
