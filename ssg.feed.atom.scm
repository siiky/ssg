; TODO: Fill in the updated fields
(module
  ssg.feed.atom
  (
   make-entry
   make-feed
   write-feed
   )

  (import
    scheme
    chicken.base
    )

  (import (prefix atom |atom:|))

  (define (make-entry #!key (id #f) (title #f) (uri #f))
    (atom:make-entry
      #:id id
      #:title (atom:make-title title)
      #:links `(,(atom:make-link #:uri uri #:type 'html))
      #:updated ""
      ))

  (define (make-feed
            #!key
            (authors #f)
            (entries '())
            (id #f)
            (title #f)
            (updated #f))
    (atom:make-feed
      #:authors (map (cute atom:make-author #:name <>) authors)
      #:entries entries
      #:id id
      #:title (atom:make-title title)
      #:updated ""))

  (define (write-feed feed path)
    (with-output-to-file
      path
      (cute atom:write-atom-doc (atom:make-atom-doc feed))))
  )
