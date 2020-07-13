(module
  ssg
  *

  (import scheme chicken.base chicken.module)

  (import
    ssg.result
    ssg.site
    )

  (define (ssg site)
    (let ((site (result-value-or-error! site)))
      (let ((css (site-css site))
            (sxml-custom-rules (site-sxml-custom-rules site))
            (file-converter (site-converter site))
            (files (site-files site))
            (index (site-index site))
            (index-maker (site-index-maker site))
            (index-path (site-index-path site)))
        (for-each (cute file-converter <> #:css css #:sxml-custom-rules sxml-custom-rules) files)
        (index-maker index index-path #:css css #:sxml-custom-rules sxml-custom-rules))))
  )
