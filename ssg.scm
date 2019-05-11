(import chicken.base)
(import chicken.file)
(import chicken.irregex)
(import chicken.pathname)
(import chicken.process-context)
(import optimism)
(import srfi-1)
(import srfi-13)
(import typed-records)
(import sxml-transforms)

; http://www.more-magic.net/docs/scheme/sxslt.pdf

(define sem-page
  `(page "./index.md"
         (toc
           (l "../")
           (l "smth.md"))
         (h1 "Some title")
         "This is just an >>example<< to show XHTML & SXML."))

(define (custom-rules)
  (define (page _ title . content)
    `(html (@ (lang "en"))
           (head (link (@ (rel "stylesheet")
                          (type "text/css")
                          (href "assets/monokai.css")))
                 (meta (@ (charset "UTF-8")))
                 (title ,title))
           (body ,content)
           (footer
             "\nplaces:\n"
             (a (@ (href "https://github.com/siiky")) "github.com")
             "\ttest\tthis\tshit")))

  (define (*text* _ str) str)
  (define (*default* . x) x)

  (define (h1 _ title) `(h1 "# " ,title "\n"))
  (define (h2 _ title) `(h2 "## " ,title "\n"))
  (define (h3 _ title) `(h3 "### " ,title "\n"))
  (define (h4 _ title) `(h4 "#### " ,title "\n"))
  (define (h5 _ title) `(h5 "##### " ,title "\n"))

  (define (l _ ref)
    `(a (@ (href ,ref)) ,ref "\n"))

  (define (toc _ . entries)
    `(ul ,entries))

  `((h1 . ,h1)
    (h2 . ,h2)
    (h3 . ,h3)
    (h4 . ,h4)
    (h5 . ,h5)
    (l . ,l)
    (toc . ,toc)
    (page . ,page)
    (*text* . ,*text*)
    (*default* . ,*default*)))

(define (main args)
  (with-output-to-file
    "test.html"
    (lambda ()
      (SRV:send-reply
        (pre-post-order (pre-post-order sem-page (custom-rules))
                        universal-conversion-rules)))))

(main (command-line-arguments))
