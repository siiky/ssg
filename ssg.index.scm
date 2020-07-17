(module
  ssg.index
  *

  (import scheme chicken.base chicken.pathname)

  (import
    srfi-1
    typed-records)

  (defstruct idx-file input-filename output-extension)
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

  (define (index-files index)
    (let* ((ret (idx-dirs index))
           (ret (map (lambda (dir) `(,(dir-name dir) . ,(directory-files dir))) ret))
           (ret (append-map
                  (lambda (dir/files)
                    (let ((directory (car dir/files))
                          (files (cdr dir/files)))
                      (map (lambda (idx-file)
                             (update-idx-file
                               idx-file
                               #:input-filename
                               (normalize-pathname (make-pathname directory (idx-file-input-filename idx-file)))))
                           files)))
                  ret)))
      ret))
  )
