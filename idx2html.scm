(module
  idx2html
  (idx2html)

  (import scheme)
  (import chicken.base)
  (import chicken.pathname)
  (import chicken.string)
  (import srfi-1)

  (define (ent-func dir tag fname . rest)
    (assert (eq? tag 'ent))
    (let* ((href (make-pathname dir (pathname-replace-extension fname "html"))))
      `(a (@ (href ,href)) ,(string-intersperse `(,fname ,@rest) "\t"))))

  (define (dir-func tag dir . ents)
    (assert (eq? tag 'dir))
    `(,(string-append "\n" dir ":\n")
       ,@(append (intersperse (map (lambda (ent) (apply ent-func `(,dir ,@ent))) ents) "\n"))))

  (define (idx-func tag page-title . dirs)
    (assert (eq? tag 'idx))
    `((h1 ,page-title)
      (body ,@(concatenate (intersperse (map (cut apply dir-func <>) dirs) '("\n"))))))

  (define (idx2html idx-file)
    (let ((idx (with-input-from-file idx-file read)))
      (let ((html (apply idx-func idx)))
        (print html)))))
