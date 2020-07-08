(module
  ssg.css
  *

  (import
    scheme
    chicken.base
    chicken.io
    )

  (define (css-string content)
    (delay content))

  (define (css-file path)
    (delay (with-input-from-file path read-string)))

  (define (css-content css)
    (and css (force css)))
  )
