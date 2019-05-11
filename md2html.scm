(module
  md2html
  (md->html)

  (import chicken.base)
  (import chicken.file)
  (import chicken.io)
  (import chicken.pathname)
  (import chicken.port)
  (import lowdown)
  (import scheme)
  (import sxml-transforms)

  (define (print-headers tree)
    ;(print "in: " tree)
    (cond
      ((null? tree)
       #t)
      ((and (list? tree)
            (symbol? (car tree))
            (memq (car tree) '(h1 h2 h3 h4 h5)))
       (print (car tree))
       (for-each print (cdr tree)))
      ((list? tree)
       (for-each print-headers tree))
      ((and (symbol? tree) (memq tree '(h1 h2 h3 h4 h5)))
       (print tree))
      (else
        #t)))

  (define (proc-tree tree)
    (markdown->sxml tree))

  (define (md->html fname)
    (let ((outfname (pathname-replace-extension fname "html"))
          (content (with-input-from-file fname read-string)))
      (let ((tree (proc-tree content)))
        ;(print-headers tree)
        `(,(with-output-to-string (lambda () (markdown->html content)))
           . ,tree)))))
