(module
  ssg.site
  *

  (import scheme chicken.base)

  (import typed-records)

  (import
    ssg.condition
    ssg.css
    ssg.index
    ssg.result)

  (defstruct site css directories do-it files index index-path markdown->html verbose)


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
                (css #f)
                (do-it #f)
                (index #f)
                (index-path "index.html")
                (markdown->html #f)
                (verbose #f))
    (let ((condition
            (lambda (argument)
              (((exn.ssg.site-condition 'ssg.site:site "Argument missing" argument))
               'site "Argument missing" argument))))
      (cond
        ((not index)
         (result-error (condition 'index)))
        ((not (string? index-path))
         (result-error (condition 'index-path)))
        ((not (procedure? markdown->html))
         (result-error (condition 'markdown->html)))
        (else
          (result-ok
            (make-site
              #:css (and css (css-content css))
              #:directories (index-directories index)
              #:do-it do-it
              #:files (index-files index)
              #:index index
              #:index-path index-path
              #:verbose verbose))))))
  )
