(module
  ssg.css
  *

  (import scheme)

  (import messages)

  (define-algebraic-type css (#:string (content string?)) (#:file (path string?)))
  (define (css-string content) ((css #:string) content))
  (define (css-file path)      ((css #:file) path))
  (define (css-content css)
    (css-case
      css
      (#:content (content) content)
      (#:file (path) 'TODO)))
  )
