(module
  ssg.converters.lowdown
  (
   idx->html
   make-sxml-custom-rules
   md->html
   md->sxml
   read-to-list
   read-to-string
   sxml->html-string
   )

  (import
    scheme
    chicken.base
    chicken.file
    chicken.io
    chicken.pathname
    chicken.port
    chicken.string
    chicken.type)

  (import lowdown
          srfi-1
          srfi-13
          sxml-transforms)

  (import
    ssg.css
    ssg.index
    )

  (: read-to-string (string -> string))
  (define (read-to-string filename)
    (with-input-from-file filename read-string))

  (: read-to-list (string -> *))
  (define (read-to-list filename)
    (with-input-from-file filename read))

  (: ->bool (* -> boolean))
  (define (->bool obj)
    (not (not obj)))

  ;;;
  ;;; SXML rules
  ;;;

  ; http://www.more-magic.net/docs/scheme/sxslt.pdf

  (define (make-sxml-custom-rules #!key (lang "en") (charset "UTF-8") (footer #f))
    (define (page _ title css . content)
      (let ((css (and css `(style ,(css-content css))))
            (footer (and footer `(footer ,footer))))
        `(html (@ (lang ,lang))
               (head (meta (@ (charset ,charset)))
                     ,css ; it seems #f is ignored
                     (title ,title))
               (body ,content)
               ,footer)))

    (define (*text* _ str) str)
    (define (*default* . x) x)
    (define (h1 _ title) `(h1 "# " ,title "\n"))
    (define (h2 _ title) `(h2 "## " ,title "\n"))
    (define (h3 _ title) `(h3 "### " ,title "\n"))
    (define (h4 _ title) `(h4 "#### " ,title "\n"))
    (define (h5 _ title) `(h5 "##### " ,title "\n"))
    (define (h6 _ title) `(h6 "###### " ,title "\n"))
    (define (p _ . content) `(,@content "\n\n"))
    (define (l _ ref) `(a (@ (href ,ref)) ,ref "\n"))
    (define (toc _ . entries) `(ul ,entries))
    (define (img _ attrs)
      (let* ((attrs (cdr attrs))
             (src (cadr (assoc 'src attrs eq?)))
             (alt (cdr  (assoc 'alt attrs eq?)))
             (alt (string-concatenate
                    `("![" ,@(or alt '("")) "](" ,@(or src '("")) ")"))))
        `(img (@ (src ,src) (alt ,alt)))))
    (define (code _ . content)
      (if (and (not (null? content))
               (any (cute string-any #\newline <>) content))
          `(code "```" ,@content "```\n")
          `(code "`" ,@content "`")))
    (define (strong _ . content)
      `(strong "**" ,@content "**"))
    (define (em/i tag . content)
      `(,tag "_" ,@content "_"))

    `((h1 . ,h1)
      (h2 . ,h2)
      (h3 . ,h3)
      (h4 . ,h4)
      (h5 . ,h5)
      (h6 . ,h6)
      (code . ,code)
      (p . ,p)
      (l . ,l)
      (toc . ,toc)
      (strong . ,strong)
      (em . ,em/i)
      (i . ,em/i)
      (img . ,img)
      (page . ,page)
      (*text* . ,*text*)
      (*default* . ,*default*)))

  ;;;
  ;;; Markdown/SXML/HTML
  ;;;

  ;; @brief Read a Markdown file into an SXML structure
  ;; @param input-filename The name of the Markdown file
  ;; @param css-filename The name of a CSS file
  ;; @param css-string The CSS style ready to be inserted in the resulting HTML
  ;; @returns A page in SXML
  (define (md->sxml input-filename #!key (css #f))
    `(page ,input-filename ,css ,(markdown->sxml (read-to-string input-filename))))

  ;; @brief Serialize an SXML structure into HTML and write it
  ;; @param output-filename The name of the output file
  ;; @param sxml The SXML structure
  (define (sxml->html-string output-filename sxml #!key (sxml-custom-rules #f))
    (with-output-to-file
      output-filename
      (lambda ()
        (SRV:send-reply
          (pre-post-order
            (if sxml-custom-rules
                (pre-post-order sxml sxml-custom-rules)
                sxml)
            universal-conversion-rules))
        (newline))))

  ;; @brief Read a Markdown file and serialize it into HTML
  ;; @param input-filename The name of the Markdown file
  ;; @param css-filename The name of a CSS file
  ;; @param css-string The CSS style ready to be inserted in the resulting HTML
  (: md->html (string string #!key (or string false) (or string false) -> void))
  (define (md->html input-filename output-filename #!key (css #f) (sxml-custom-rules #f))
    (sxml->html-string
      output-filename
      (md->sxml input-filename #:css css)
      #:sxml-custom-rules sxml-custom-rules))

  ;;;
  ;;; Index files
  ;;;

  (define (ent-func dir ent)
    (let ((fname (ent-name ent))
          (date (ent-date ent))
          (title (ent-title ent)))
      (let* ((href (make-pathname dir (pathname-replace-extension fname "html"))))
        `(a (@ (href ,href)) ,(string-intersperse `(,fname ,date ,title) "\t")))))

  (define (dir-func dir)
    (let ((dir (dir-name dir))
          (ents (dir-ents dir)))
      `(,(string-append "\n" dir ":\n")
         ,@(append (intersperse (map (cute ent-func dir <>) (filter (complement ent-wip?) ents)) "\n")))))

  (define (idx-func index #!key (css #f))
    (let ((title (idx-title index))
          (dirs (idx-dirs index)))
      `(page ,title ,css
             (h1 ,title)
             ,(concatenate (intersperse (map dir-func (filter (complement dir-wip?) dirs)) '("\n"))))))

  (define (idx->html index index-path #!key (css #f) (sxml-custom-rules #f))
    (let* ((html (idx-func index #:css css)))
      (sxml->html-string index-path html #:sxml-custom-rules sxml-custom-rules)))
  )
