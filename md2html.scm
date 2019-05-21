(module
  md2html
  (md->html read-to-string)

  (import chicken.base)
  (import chicken.file)
  (import chicken.io)
  (import chicken.pathname)
  (import chicken.port)
  (import chicken.type)
  (import lowdown)
  (import scheme)
  (import srfi-1)
  (import srfi-13)
  (import sxml-transforms)

  ; http://www.more-magic.net/docs/scheme/sxslt.pdf

  (define (make-custom-rules)
    (define (page _ title css . content)
      (let ((css (and css `(style ,css))))
        `(html (@ (lang "en"))
               (head ,css ; it seems #f is ignored
                     (meta (@ (charset "UTF-8")))
                     (title ,title))
               (body ,content)
               (footer
                 "\nplaces:\n"
                 (a (@ (href "https://github.com/siiky")) "github.com")
                 "\ttest\tthis\tshit"))))

    (define (*text* _ str) str)
    (define (*default* . x) x)
    (define (h1 _ title) `(h1 "# " ,title "\n"))
    (define (h2 _ title) `(h2 "## " ,title "\n"))
    (define (h3 _ title) `(h3 "### " ,title "\n"))
    (define (h4 _ title) `(h4 "#### " ,title "\n"))
    (define (h5 _ title) `(h5 "##### " ,title "\n"))
    (define (h6 _ title) `(h5 "###### " ,title "\n"))
    (define (p _ . content) `(,@content "\n"))
    (define (l _ ref) `(a (@ (href ,ref)) ,ref "\n"))
    (define (toc _ . entries) `(ul ,entries))
    (define (img _ attrs)
      (let ((attrs (cdr attrs)))
        (let ((src (cadr (assoc 'src attrs eq?)))
              (alt (cdr (assoc 'alt attrs eq?))))
          (let ((alt (string-concatenate `("![" ,@(or alt '("")) "](" ,@(or src '("")) ")"))))
            `(img (@ (src ,src) (alt ,alt)))))))
    (define (code _ . content)
      (if (and (not (null? content))
               (any (lambda (str) (string-any #\newline str)) content))
          `(code "```" ,@content "```\n")
          `(code "`" ,@content "`")))

    `((h1 . ,h1)
      (h2 . ,h2)
      (h3 . ,h3)
      (h4 . ,h4)
      (h5 . ,h5)
      (code . ,code)
      (p . ,p)
      (l . ,l)
      (toc . ,toc)
      (img . ,img)
      (page . ,page)
      (*text* . ,*text*)
      (*default* . ,*default*)))

  (define custom-rules (make-custom-rules))

  (: read-to-string (string -> string))
  (define (read-to-string filename)
    (with-input-from-file filename read-string))

  (: md->html (string #!key string string -> void))
  (define (md->html input-filename
                    #!key
                    (css-filename #f)
                    (css-string #f))
    (let* ((css (or css-string
                    (and css-filename
                         (file-exists? css-filename)
                         (read-to-string css-filename))))
           (output-filename (pathname-replace-extension input-filename "html")))
      (with-output-to-file
        output-filename
        (lambda ()
          (SRV:send-reply
            (pre-post-order
              (pre-post-order
                `(page ,input-filename ,css ,(markdown->sxml (read-to-string input-filename)))
                custom-rules)
              universal-conversion-rules))
          (newline))))))
