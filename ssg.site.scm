(module
  ssg.site
  *

  (import scheme)

  (import typed-records)

  (import
    ssg.css
    ssg.index
    ssg.result)

  ;;;
  ;;; Site Definition
  ;;;

  (defstruct site css directories do-it files index index-path markdown->html verbose)

  (define (site #!key
		(css #f)
		(do-it #f)
		(index #f)
		(index-path "index.html")
		(markdown->html #f)
		(verbose #f))
    (cond
      ((not index)
       (result-error 'index))
      ((not (string? index-path))
       (result-error 'index-path))
      ((not (procedure? markdown->html))
       (result-error 'markdown->html))
      (else
	(result-ok
	  (make-site
	    #:css (and css (css-content css))
	    #:directories (index-directories index)
	    #:do-it do-it
	    #:files (index-files index)
	    #:index index
	    #:index-path index-path
	    #:verbose verbose)))))
  )
