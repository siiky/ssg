(module ssg.result
  (
   exception->result
   handle-result
   result-bind
   result-ref
   result/error
   result/error?
   result/ok
   result/ok?
   result?
   )

  (import scheme)

  (import
    (rename
      (only srfi-189
            either-bind
            either-ref
            either?
            exception->either
            left
            left?
            right
            right?)
      (either-bind       result-bind)
      (either-ref        result-ref)
      (either?           result?)
      (exception->either exception->result)
      (left              result/error)
      (left?             result/error?)
      (right             result/ok)
      (right?            result/ok?)))

  (define (handle-result result ok-proc error-proc)
   (result-ref result error-proc ok-proc))
  )
