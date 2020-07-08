(module
  ssg.site
  *

  (import scheme chicken.base chicken.pathname chicken.module)

  (import srfi-1 typed-records)

  (import
    ssg.condition
    ssg.css
    ssg.index
    ssg.result)

  (defstruct site
             converter
             css
             directories
             do-it
             files
             index
             index-maker
             index-path
             sxml-custom-rules
             verbose
             )

  (defstruct converter-table-entry input-extension output-extension converter)

  (define-syntax make-converter-table
    (syntax-rules ()
      ((make-converter-table (from to ->) ...)
       `(,(make-converter-table-entry #:input-extension from #:output-extension to #:converter ->) ...))))

  (define ((table->converter converter-table) input-filename #!key (css #f) (sxml-custom-rules #f))
    (define (converter-not-found input-filename output-filename . _)
      (result-error 'converter-not-found))

    (let* ((input-extension (pathname-extension input-filename))
           (converter-entry (member input-extension converter-table
                                    (lambda (iext tbl-ent)
                                      (string=? (converter-table-entry-input-extension tbl-ent) iext))))
           (converter-entry (and converter-entry (car converter-entry)))
           (output-extension (if converter-entry (converter-table-entry-output-extension converter-entry) converter-not-found))
           (output-filename (pathname-replace-extension input-filename output-extension))
           (converter (if converter-entry (converter-table-entry-converter converter-entry) converter-not-found)))

      (converter input-filename output-filename #:css css #:sxml-custom-rules sxml-custom-rules)))

  (define (((exn.ssg.site-condition exn-location exn-message exn-arguments)) site-location site-message site-argument)
    (condition
      (exn-condition
        #:arguments exn-arguments
        #:location exn-location
        #:message exn-message)

      (ssg-condition)

      (site-condition
        #:argument site-argument
        #:location site-location
        #:message site-message)))

  (define (site #!key
                (converter-table #f)
                (css #f)
                (do-it #f)
                (index #f)
                (index-maker #f)
                (index-path "index.html")
                (sxml-custom-rules #f)
                (verbose #f))
    (let ((condition
            (lambda (argument)
              (((exn.ssg.site-condition 'ssg.site:site "Argument missing" argument))
               'site "Argument missing" argument))))
      (cond
        ((not (list? converter-table))     (result-error (condition 'converter-table)))
        ((not (procedure? index-maker))    (result-error (condition 'index-maker)))
        ((not (string? index-path))        (result-error (condition 'index-path)))
        ((not index)                       (result-error (condition 'index)))
        (else
          (result-ok
            (make-site
              #:converter (table->converter converter-table)
              #:css css
              #:directories (index-directories index)
              #:do-it do-it
              #:files (index-files index)
              #:index index
              #:index-maker index-maker
              #:index-path index-path
              #:sxml-custom-rules sxml-custom-rules
              #:verbose verbose))))))
  )
