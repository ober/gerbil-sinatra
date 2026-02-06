(import :std/markup/sxml
        :std/markup/html
        ./context
        ./app)

(export render-sxml
        render-file-template
        render-string
        views-path)

;; Render an SXML tree to an HTML string.
(def (render-sxml sxml-tree)
  (sxml->html sxml-tree))

;; Resolve view file path relative to the views directory.
(def (views-path name (ext ".html"))
  (let* ((a (current-app))
         (views-dir (if a (app-setting a "views") "./views")))
    (string-append views-dir "/" (if (symbol? name) (symbol->string name) name) ext)))

;; Render a file template with simple variable substitution.
;; Template uses {{key}} placeholders, vars is a hash-table.
(def (render-file-template name vars)
  (let* ((path (views-path name))
         (template-str (read-file-string path)))
    (render-string template-str vars)))

;; Render a string template with {{key}} placeholder substitution.
;; vars is a hash-table mapping string keys to string values.
(def (render-string template-str vars)
  (let ((len (string-length template-str))
        (out (open-output-string)))
    (let loop ((i 0))
      (cond
        ;; End of string
        ((>= i len)
         (get-output-string out))
        ;; Check for {{ start
        ((and (< (+ i 1) len)
              (char=? (string-ref template-str i) #\{)
              (char=? (string-ref template-str (+ i 1)) #\{))
         (let ((end-pos (find-closing-braces template-str (+ i 2) len)))
           (if end-pos
             (let* ((key (str-trim (substring template-str (+ i 2) end-pos)))
                    (value (or (hash-get vars key) "")))
               (write-string (if (string? value) value (object->string value)) out)
               (loop (+ end-pos 2)))
             (begin
               (write-char (string-ref template-str i) out)
               (loop (+ i 1))))))
        ;; Normal character
        (else
         (write-char (string-ref template-str i) out)
         (loop (+ i 1)))))))

;; Find the position of }} in template string
(def (find-closing-braces str start len)
  (let loop ((i start))
    (cond
      ((>= (+ i 1) len) #f)
      ((and (char=? (string-ref str i) #\})
            (char=? (string-ref str (+ i 1)) #\}))
       i)
      (else (loop (+ i 1))))))

;; Read entire file as string
(def (read-file-string path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((chars []))
        (let ((ch (read-char port)))
          (if (eof-object? ch)
            (list->string (reverse chars))
            (loop (cons ch chars))))))))

;; Helper: object to string
(def (object->string obj)
  (call-with-output-string (lambda (p) (display obj p))))

;; Helper: trim whitespace
(def (str-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref s i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring s start end)))
