(import :std/pregexp
        ./route)

(export run-before-filters
        run-after-filters
        filter-matches?)

;; Run before filters for the given method and path.
;; Filters are stored as (cons pattern-or-#f handler).
(def (run-before-filters app method path)
  (run-filters (hash-ref app 'before-filters) method path))

;; Run after filters for the given method and path.
(def (run-after-filters app method path)
  (run-filters (hash-ref app 'after-filters) method path))

;; Internal: run a list of filters
(def (run-filters filters method path)
  (for-each
    (lambda (filter)
      (let ((pattern (car filter))
            (handler (cdr filter)))
        (when (or (not pattern)
                  (filter-matches? pattern path))
          (handler))))
    filters))

;; Check if a pattern matches the given path.
(def (filter-matches? pattern path)
  (let-values (((rx _names) (compile-route-pattern pattern)))
    (and (pregexp-match rx path) #t)))
