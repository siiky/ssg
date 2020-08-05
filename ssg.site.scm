(module
  ssg.site
  *

  (import
    scheme
    chicken.base
    chicken.file
    chicken.file.posix
    chicken.pathname)

  (import
    srfi-1
    typed-records)

  (import
    ssg.condition
    ssg.css
    ssg.feed
    ssg.index
    ssg.result)

  (defstruct site
             converter
             css
             directories
             feed
             files
             index
             index-maker
             index-path
             sxml-custom-rules
             )

  (defstruct converter-table-entry input-extension output-extension converter)

  (define-syntax make-converter-table
    (syntax-rules ()
      ((make-converter-table (from to ->) ...)
       `(,(make-converter-table-entry #:input-extension from #:output-extension to #:converter ->) ...))))

  ; NOTE: Use rest arguments `kargs` instead of specifying key arguments here
  ;       to support various converters with different key arguments
  (define ((table->converter converter-table) idx-file . kargs)
    (define (converter-not-found input-filename output-filename . _)
      (result-error 'converter-not-found))

    (let* ((input-filename (idx-file-input-filename idx-file))
           (output-extension (idx-file-output-extension idx-file))
           (input-extension (pathname-extension input-filename))
           (converter-entry (member input-extension converter-table
                                    (lambda (iext tbl-ent)
                                      (string=? (converter-table-entry-input-extension tbl-ent) iext))))
           (converter-entry (and converter-entry (car converter-entry)))
           (output-filename (pathname-replace-extension input-filename output-extension))
           (converter (if converter-entry (converter-table-entry-converter converter-entry) converter-not-found)))

      (apply converter input-filename output-filename #:resource-path (pathname-directory input-filename) kargs)))

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

  (define (index-entries-for-feed index)
    (filter identity
            (index-map-all-entries
              (lambda (dir ent)
                (and (not (or (dir-wip? dir) (ent-wip? ent)))
                     (feed-entry (ent-title ent)
                                 (relative-path (dir-name dir)
                                                (ent-name ent)))))
              index)))

  (define (site
            #!key
            (converter-table #f)
            (css #f)
            (feed #f)
            (force-redo? #f)
            (index #f)
            (index-maker #f)
            (index-path "index.html")
            (sxml-custom-rules #f))

    (define (should-process-file? idx-file)
      (let ((input-filename (idx-file-input-filename idx-file))
            (output-extension (idx-file-output-extension idx-file)))
        (let ((output-filename (pathname-replace-extension input-filename output-extension)))
          (or (not (file-exists? output-filename))
              (> (file-modification-time input-filename)
                 (file-modification-time output-filename))))))

    (define (condition argument)
      (((exn.ssg.site-condition 'ssg.site:site "Argument missing" argument))
       'site "Argument missing" argument))

    (cond
      ((not (list? converter-table))              (result-error (condition 'converter-table)))
      ((not (or (not feed) (feed-options? feed))) (result-error (condition 'feed)))
      ((not (procedure? index-maker))             (result-error (condition 'index-maker)))
      ((not (string? index-path))                 (result-error (condition 'index-path)))
      ((not (idx? index))                         (result-error (condition 'index)))
      (else
        (let* ((files (index-files index))
               (files (if force-redo? files (filter should-process-file? files)))
               (feed (update-feed-options
                       feed
                       #:title (idx-title index)
                       #:entries (index-entries-for-feed index))))
          (result-ok
            (make-site
              #:converter (table->converter converter-table)
              #:css css
              #:directories (index-directories index)
              #:feed feed
              #:files files
              #:index index
              #:index-maker index-maker
              #:index-path index-path
              #:sxml-custom-rules sxml-custom-rules))))))
  )
