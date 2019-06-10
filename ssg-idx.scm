(import chicken.process-context)
(import md2html)

(define (usage pn)
  (print pn " INDEX_FILE"))

(define (main args)
  (if (or (null? args) (not (null? (cdr args))))
      (usage (program-name))
      (idx->html (car args))))

(main (command-line-arguments))
