(module
  ssg.feed
  (
   feed-entry
   feed-options
   feed-options->feed
   feed-options-path
   feed-options-type
   feed-options?
   update-feed-options
   write-feed
   )

  (import scheme chicken.pathname)

  (import typed-records)

  (import
    (prefix ssg.feed.atom |atom:|)
    ssg.result)

  (defstruct feed-entry
             title
             path
             )

  (defstruct feed-options
             authors
             entries
             id
             path
             title
             type
             )

  (defstruct feed feed path type)

  (define (feed-entry title path)
    (make-feed-entry #:title title #:path path))

  (define (feed-options #!key (authors "") (id #f) (path "atom.xml") (type 'atom))
    (make-feed-options #:authors authors #:id id #:path path #:type type))

  (define (feed-options->feed fo #!key (extension "html"))
    (let* ((type (feed-options-type fo))
           (path (feed-options-path fo))
           (feed
             (case type
               ((atom)
                (let ((authors (feed-options-authors fo))
                      (entries (feed-options-entries fo))
                      (id (feed-options-id fo))
                      (path (feed-options-path fo))
                      (title (feed-options-title fo)))
                  (let ((entry-maker
                          (lambda (fe)
                            (let ((id (string-append id "/" (feed-entry-path fe)))
                                  (title (feed-entry-title fe)))
                             (let ((uri (pathname-replace-extension id extension)))
                              (atom:make-entry #:id id #:title title #:uri uri))))))
                    (let ((authors (if (string? authors) `(,authors) authors))
                          (entries (map entry-maker entries)))
                      (atom:make-feed
                        #:authors authors
                        #:entries entries
                        #:id id
                        #:path path
                        #:title title
                        )))))

               (else
                 (result-error 'badargs)))))
      (make-feed
        #:feed feed
        #:path path
        #:type type)))

  (define (write-feed feed)
    (case (feed-type feed)
      ((atom)
       (atom:write-feed (feed-feed feed) (feed-path feed)))

      (else
        (result-error 'badargs))))
  )
