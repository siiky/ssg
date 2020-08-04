(module
  ssg.css
  *

  (import
    scheme
    chicken.base
    chicken.io
    )

  (import typed-records)

  (defstruct %css path content)

  (define (css-string content)
    (make-%css #:path #f #:content (delay content)))

  (define (css-file path)
    (make-%css #:path path #:content (delay (with-input-from-file path read-string))))

  (define (css-path css)
    (and css (%css-path css)))

  (define (css-content css)
    (and css (force (%css-content css))))
  )
