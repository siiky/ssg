(module
  ssg.condition
  *

  (import scheme chicken.base chicken.condition chicken.module)
  (reexport (only chicken.condition condition))

  (import srfi-1)

  (define-constant property-not-given-default (gensym 'no-prop))

  (define (property-given? property)
    (not (eq? property property-not-given-default)))

  (define (prepare-properties properties)
    (concatenate (filter (o property-given? cadr) properties)))

  (define-syntax define-condition-maker
    (syntax-rules ()
      ((define-condition-maker maker kind prop ...)
       (define (maker #!key (prop property-not-given-default) ...)
         `(kind ,@(prepare-properties `((prop ,prop) ...)))))))

  (define-condition-maker exn-condition exn location message arguments)
  (define-condition-maker ssg-condition ssg)
  (define-condition-maker site-condition site location message argument)
  )
