(import chicken.process-context)
(import idx2html)

(define (usage pn)
  (print pn " INDEX_FILE"))

(define (main args)
  (if (or (null? args) (not (null? (cdr args))))
      (usage (program-name))
      (idx2html (car args))))

(main (command-line-arguments))
