(module
  ssg
  *

  (import scheme chicken.base chicken.module)

  (import
    ssg.feed
    ssg.result
    ssg.site
    )

  (define (ssg site)
    (define ((convert-with converter . kargs) idx-file)
     (apply converter idx-file kargs))

    (let ((site (result-value-or-error! site)))
      (let ((css (site-css site))
            (sxml-custom-rules (site-sxml-custom-rules site))
            (file-converter (site-converter site))
            (files (site-files site))
            (index (site-index site))
            (index-maker (site-index-maker site))
            (index-path (site-index-path site))
            (feed (site-feed site))
            (extensions '("html")))

        (for-each (convert-with file-converter #:css css #:sxml-custom-rules sxml-custom-rules) files)
        (index-maker index index-path #:css css #:sxml-custom-rules sxml-custom-rules)

        (when feed
          (let ((write-single-feed
                  (lambda (extension)
                    (let* ((feed (feed-options->feed feed #:extension extension))
                           (path (feed-path feed)))
                      (write-feed feed path extension)))))
            (for-each write-single-feed extensions))))))
  )
