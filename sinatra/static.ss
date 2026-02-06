(import :std/net/httpd
        ./mime
        ./app)

(export try-static-file
        serve-static-file)

;; Check if a file exists in the public folder for the given request path.
;; Returns the full file path or #f.
(def (try-static-file app path)
  (with-catch
    (lambda (e) #f)
    (lambda ()
      (let* ((public-dir (or (app-setting app "public-folder") "./public"))
             (normalized-public (path-normalize public-dir))
             ;; Build the full path: public-dir + request path
             (full-path (path-expand
                          (string-append "." path)
                          public-dir))
             (normalized-full (path-normalize full-path)))
        ;; Security: ensure resolved path is within public-dir (prevent traversal)
        (and (string-prefix? normalized-public normalized-full)
             (file-exists? normalized-full)
             (not (file-directory? normalized-full))
             normalized-full)))))

;; Serve a static file directly via httpd response.
;; Uses http-response-file which handles the file efficiently.
(def (serve-static-file full-path httpd-res)
  (let* ((ct (or (mime-type-for full-path) "application/octet-stream"))
         (headers [(cons "Content-Type" ct)]))
    (http-response-file httpd-res headers full-path)))

;; Helper: check if str starts with prefix
(def (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;; Helper: check if path is a directory
(def (file-directory? path)
  (and (file-exists? path)
       (eq? (file-type path) 'directory)))
