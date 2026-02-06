(export mime-type-for
        mime-type-for-ext
        mime-type-sym)

(def +mime-types+
  (hash
   ;; Text
   ("html"    "text/html")
   ("htm"     "text/html")
   ("css"     "text/css")
   ("csv"     "text/csv")
   ("txt"     "text/plain")
   ("text"    "text/plain")
   ("tsv"     "text/tab-separated-values")
   ("ics"     "text/calendar")
   ("vcf"     "text/vcard")
   ("xml"     "text/xml")
   ("rtf"     "text/rtf")
   ("md"      "text/markdown")

   ;; JavaScript / JSON
   ("js"      "application/javascript")
   ("mjs"     "application/javascript")
   ("json"    "application/json")
   ("map"     "application/json")
   ("jsonld"  "application/ld+json")

   ;; Images
   ("png"     "image/png")
   ("jpg"     "image/jpeg")
   ("jpeg"    "image/jpeg")
   ("gif"     "image/gif")
   ("svg"     "image/svg+xml")
   ("ico"     "image/x-icon")
   ("bmp"     "image/bmp")
   ("webp"    "image/webp")
   ("avif"    "image/avif")
   ("tiff"    "image/tiff")
   ("tif"     "image/tiff")

   ;; Audio
   ("mp3"     "audio/mpeg")
   ("ogg"     "audio/ogg")
   ("wav"     "audio/wav")
   ("flac"    "audio/flac")
   ("aac"     "audio/aac")
   ("m4a"     "audio/mp4")
   ("weba"    "audio/webm")
   ("mid"     "audio/midi")
   ("midi"    "audio/midi")

   ;; Video
   ("mp4"     "video/mp4")
   ("webm"    "video/webm")
   ("ogv"     "video/ogg")
   ("avi"     "video/x-msvideo")
   ("mpeg"    "video/mpeg")
   ("mov"     "video/quicktime")

   ;; Fonts
   ("woff"    "font/woff")
   ("woff2"   "font/woff2")
   ("ttf"     "font/ttf")
   ("otf"     "font/otf")
   ("eot"     "application/vnd.ms-fontobject")

   ;; Application
   ("pdf"     "application/pdf")
   ("zip"     "application/zip")
   ("gz"      "application/gzip")
   ("tar"     "application/x-tar")
   ("bz2"     "application/x-bzip2")
   ("7z"      "application/x-7z-compressed")
   ("rar"     "application/vnd.rar")
   ("xls"     "application/vnd.ms-excel")
   ("xlsx"    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
   ("doc"     "application/msword")
   ("docx"    "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
   ("ppt"     "application/vnd.ms-powerpoint")
   ("pptx"    "application/vnd.openxmlformats-officedocument.presentationml.presentation")
   ("wasm"    "application/wasm")
   ("bin"     "application/octet-stream")
   ("exe"     "application/octet-stream")
   ("dll"     "application/octet-stream")
   ("deb"     "application/octet-stream")
   ("dmg"     "application/octet-stream")
   ("iso"     "application/octet-stream")
   ("swf"     "application/x-shockwave-flash")
   ("atom"    "application/atom+xml")
   ("rss"     "application/rss+xml")
   ("xhtml"   "application/xhtml+xml")
   ("yaml"    "application/x-yaml")
   ("yml"     "application/x-yaml")
   ("toml"    "application/toml")))

;; Symbol shorthand table
(def +mime-sym+
  (hash
   (json    "application/json")
   (html    "text/html")
   (xml     "text/xml")
   (text    "text/plain")
   (css     "text/css")
   (js      "application/javascript")
   (csv     "text/csv")
   (pdf     "application/pdf")
   (svg     "image/svg+xml")
   (png     "image/png")
   (jpg     "image/jpeg")
   (gif     "image/gif")
   (zip     "application/zip")
   (binary  "application/octet-stream")
   (form    "application/x-www-form-urlencoded")
   (multipart "multipart/form-data")))

;; Extract extension from a file path (without the dot)
(def (path-extension path)
  (let ((dot (let loop ((i (- (string-length path) 1)))
               (cond
                 ((< i 0) #f)
                 ((char=? (string-ref path i) #\.) i)
                 ((char=? (string-ref path i) #\/) #f)
                 (else (loop (- i 1)))))))
    (and dot (substring path (+ dot 1) (string-length path)))))

;; (mime-type-for "/path/to/file.html") => "text/html"
(def (mime-type-for path)
  (let ((ext (path-extension path)))
    (and ext (hash-get +mime-types+ (string-downcase ext)))))

;; (mime-type-for-ext "html") => "text/html"
(def (mime-type-for-ext ext)
  (hash-get +mime-types+ (string-downcase ext)))

;; (mime-type-sym 'json) => "application/json"
(def (mime-type-sym sym)
  (or (hash-get +mime-sym+ sym)
      (error "Unknown MIME type symbol" sym)))
