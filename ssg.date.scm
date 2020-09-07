(module
  ssg.date
  (date)

  (import
    scheme)

  (import
    rfc3339)

  (define (date year month day #!key (hours 0) (minutes 0) (seconds 0) (fractions 0) (offset 0))
    (make-rfc3339 year month day hours minutes seconds fractions offset))
  )
