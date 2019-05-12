(import chicken.base)
(import chicken.pathname)
(import chicken.process-context)

(import md2html)

(define (main args)
  (md->html (car args) (pathname-replace-extension (car args) "html")))

(main (command-line-arguments))
