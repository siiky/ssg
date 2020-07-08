(module
  ssg.index
  *

  (import scheme chicken.base)

  (import
    srfi-1
    typed-records)

  (defstruct idx title dirs)
  (defstruct dir name ents wip?)
  (defstruct ent name date title wip?)

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
      (let ((name (car args))
            (date (cadr args))
            (title (caddr args)))
        (make-ent #:name name #:date date #:title title #:wip? wip?))))

  (define (index-directories index)
    (map dir-name (idx-dirs index)))

  (define (directory-files directory)
    (map ent-name (dir-ents directory)))

  (define (index-files index)
    (let* ((ret (idx-dirs index))
           (ret (map (lambda (d) (cons d (directory-files d))) ret))
           (ret (append-map
                  (lambda (dir/files)
                    (let ((directory (dir-name (car dir/files)))
                          (files (cdr dir/files)))
                      (map (cute string-append directory "/" <>) files)))
                  ret)))
      ret))
  )
