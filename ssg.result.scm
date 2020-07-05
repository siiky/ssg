(module
  ssg.result
  (
   case-variant
   result
   result-case
   result-error
   result-error-cause
   result-error?
   result-ok
   result-ok-value
   result-ok?
   result-value-or-error!
   )

  (import scheme chicken.condition)

  (import messages)

  (define (any? _) #t)

  (define-algebraic-type
    result
    (#:ok (value any?))
    (#:error (cause any?)))

  (define (result-error? result)
    (result-case
      result
      (#:ok (_) #f)
      (#:error (_) #t)))

  (define (result-ok? result)
    (result-case
      result
      (#:ok (_) #t)
      (#:error (_) #f)))

  (define (result-error cause) ((result #:error) cause))
  (define (result-ok value) ((result #:ok) value))

  (define (result-error-cause result)
    (result-case result (#:error (cause) cause)))

  (define (result-ok-value result)
    (result-case result (#:ok (value) value)))

  (define (result-value-or-error! result #!optional (error signal))
    (result-case
      result
      (#:ok (site) site)
      (#:error (reason) (error reason))))
  )
