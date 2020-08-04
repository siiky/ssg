(module
  ssg.converters.pandoc
  (
   md->html
   )
  (import
    scheme
    chicken.base
    chicken.port
    chicken.process)

  (import
    srfi-1)

  (import
    ssg.css
    ssg.result)

  ; TODO: Add the footer.
  (define (pandoc #!key (cmd "pandoc") (from #f) (to #f) (input #f) (output #f) (css #f) (title #f) (resource-path #f))
    (define (extra-default-options args)
      `("--self-contained" "-s" "--html-q-tags" . ,args))

    (define (add-input input args)
      (if input (cons input args) args))

    (define (make-args args)
      ; NOTE: Elements are of the form `(option value)`, and `value` may be #f.
      (concatenate (filter cadr args)))

    (define (metadata-option name value)
      ; TODO: Does it need quoting? Since this is gonna be one of the elements
      ;       of the argv, I think not.
      ;(and value (string-append name "=\"" value "\"")))
      (and value (string-append name "=" value)))

    ; TODO: Do not silently fail when something fails; e.g., `(css-path css)`
    ;       when `css` is `(css-string ...)`
    (let* ((args (make-args `(("-f" ,from)
                              ("-t" ,to)
                              ("-o" ,output)
                              ("-c" ,(css-path css)) ; css-path is #f safe
                              ;("-M" ,(metadata-option "pagetitle" title))
                              ("-M" ,(metadata-option "title" title))
                              ("--resource-path" ,resource-path)
                              ;("--resource-path" ".")
                              )))
           (args (extra-default-options args))
           (args (add-input input args)))
      (process-run cmd args)))

  (define ((-> from to) input-filename output-filename #!key (css #f) (title #f))
    (let ((pid (pandoc #:from from #:to to #:input input-filename #:output output-filename #:css css #:title title)))
      (let-values (((pid normal-exit? status) (process-wait pid)))
        (if normal-exit?
            (result-ok #f)
            (result-error status)))))

  (define (md->html input-filename output-filename #!key (css #f) (title #f))
    ((-> "markdown" "html") input-filename output-filename #:css css #:title title))

  ; TODO: Build a Markdown document from the index and pass it through the
  ;       stdin to pandoc. How to write to stdin?
  ; Some options:
  ;  * chicken.process.with-input-from-pipe or
  ;    chicken.process.with-output-to-pipe (not sure which)
  ;  * chicken.process.process instead of chicken.process.process-run
  ;(define (idx->html index index-path #!key (css #f))
  ;  (let ((markdown ""))
  ;    (with-input-from-string markdown (lambda () ((-> "markdown" "html") #f "index.html" #:css css)))))
  )
