(module
  ssg.feed
  (
   feed-entry
   feed-options
   feed-options->feed
   feed-options-type
   feed-options?
   feed-path
   feed-type
   generate-feed-path
   update-feed-options
   write-feed
   )

  (import scheme chicken.pathname chicken.string)

  (import srfi-13 typed-records)

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
                        #:title title)))))

               (else
                 (result-error 'badargs)))))
      (make-feed
        #:feed feed
        #:path path
        #:type type)))

  (define (generate-feed-path . components)
    (string-join (map ->string components) "."))

  (define (write-feed feed path #!optional (extension "html"))
    (let* ((type (feed-type feed))
           (path (or path (generate-feed-path type extension "xml"))))
      (case type
        ((atom)
         (atom:write-feed (feed-feed feed) path))

        (else
          (result-error 'badargs)))))
  )
