(module
  ssg.converters.pandoc
  (
   md->html
   )
  (import
    scheme
    chicken.base
    chicken.process)

  (import
    srfi-1)

  (import
    ssg.result)

  (define (pandoc #!key (from #f) (to #f) (input #f) (output #f))
    (define (add-input input args)
      (if input (cons input args) args))

    (define (make-args args)
      (append-map (lambda (elem) (list (car elem) (cdr elem)))
                  (filter cdr args)))

    (let ((args (make-args `(("-f" . ,from) ("-t" . ,to) ("-o" . ,output)))))
      (process-run "pandoc" (add-input input args))))

  (define ((-> from to) input-filename output-filename #!key (css #f))
    ; TODO: How to use CSS
    (let ((pid (pandoc #:from from #:to to #:input input-filename #:output output-filename)))
      (let-values (((pid normal-exit? status) (process-wait pid)))
        (if normal-exit?
            (result-ok #f)
            (result-error status)))))

  (define (md->html input-filename output-filename #!key (css #f) (sxml-custom-rules #f))
    ((-> "markdown" "html") input-filename output-filename #:css css))
  )
