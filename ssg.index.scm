(module
  ssg.index
  *

  (import
    scheme
    chicken.base
    chicken.pathname)

  (import
    srfi-1
    typed-records)

  (import
    ssg.feed)

  (defstruct idx-file input-filename output-extension title)
  (defstruct idx title dirs)
  (defstruct dir name ents wip?)
  (defstruct ent file name date title wip?)

  (define (sym-wip? x) (eq? x 'wip))

  (define (idx title . dirs)
    (make-idx #:title title #:dirs (filter dir? dirs)))

  (define (dir . args)
    (let* ((wip? (or (any sym-wip? args) (every ent-wip? (filter ent? args))))
           (args (if wip? (filter (complement sym-wip?) args) args)))
      (let ((name (car args))
            (args (cdr args)))
        (let ((ents (filter ent? args)))
          (make-dir #:name name #:ents ents #:wip? wip?)))))

  (define (ent . args)
    (let* ((wip? (any sym-wip? args))
           (args (if wip? (filter (complement sym-wip?) args) args)))
      (let ((name/ext (car args))
            (date (cadr args))
            (title (caddr args)))
        (let* ((name (if (string? name/ext) name/ext (car name/ext)))
               (output-extension (if (pair? name/ext) (cdr name/ext) "html"))
               (file (make-idx-file #:input-filename name #:output-extension output-extension)))
          (make-ent #:file file #:name name #:date date #:title title #:wip? wip?)))))

  (define (index-directories index)
    (map dir-name (idx-dirs index)))

  (define (directory-files directory)
    (map ent-file (dir-ents directory)))

  (define (relative-path directory filename)
    (normalize-pathname (make-pathname directory filename)))

  (define (index-files index)
    (index-map-all-entries
      (lambda (dir ent)
        (let ((idx-file (ent-file ent)))
          (update-idx-file
            idx-file #:title (ent-title ent)
            #:input-filename
            (relative-path (dir-name dir)
                           (idx-file-input-filename idx-file)))))
      index))

  (define (index-map-all-entries func index)
    (append-map
      (lambda (dir)
        (map (lambda (ent) (func dir ent))
             (dir-ents dir)))
      (idx-dirs index)))
  )
