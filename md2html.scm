(module
  md2html
  (
   idx->html
   md->html
   md->sxml
   read-to-list
   read-to-string
   sxml->html-string
   )

  (import chicken.base)
  (import chicken.file)
  (import chicken.io)
  (import chicken.pathname)
  (import chicken.port)
  (import chicken.string)
  (import chicken.type)
  (import lowdown)
  (import scheme)
  (import srfi-1)
  (import srfi-13)
  (import sxml-transforms)

  (: read-to-string (string -> string))
  (define (read-to-string filename)
    (with-input-from-file filename read-string))

  (: read-to-list (string -> *))
  (define (read-to-list filename)
    (with-input-from-file filename read))

  ;;;
  ;;; SXML rules
  ;;;

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
                 (a (@ (href "https://siiky.github.io")) "Go home!") "\n"
                 (a (@ (href "https://github.com/siiky")) "GitHub")))))

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
               (any (lambda (str) (string-any #\newline str)) content))
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

  (define custom-rules (make-custom-rules))

  ;;;
  ;;; Markdown/SXML/HTML
  ;;;

  ;; @brief Read a Markdown file into an SXML structure
  ;; @param input-filename The name of the Markdown file
  ;; @param css-filename The name of a CSS file
  ;; @param css-string The CSS style ready to be inserted in the resulting HTML
  ;; @returns A page in SXML
  (define (md->sxml input-filename
                    #!key
                    (css-filename #f)
                    (css-string #f))
    (let ((css (or css-string
                   (and css-filename
                        (file-exists? css-filename)
                        (read-to-string css-filename)))))
      `(page ,input-filename ,css ,(markdown->sxml (read-to-string input-filename)))))

  ;; @brief Serialize an SXML structure into HTML and write it
  ;; @param output-filename The name of the output file
  ;; @param sxml The SXML structure
  (define (sxml->html-string output-filename sxml)
    (with-output-to-file
      output-filename
      (lambda ()
        (SRV:send-reply (pre-post-order (pre-post-order sxml custom-rules) universal-conversion-rules))
        (newline))))

  ;; @brief Read a Markdown file and serialize it into HTML
  ;; @param input-filename The name of the Markdown file
  ;; @param css-filename The name of a CSS file
  ;; @param css-string The CSS style ready to be inserted in the resulting HTML
  (: md->html (string #!key string string -> void))
  (define (md->html input-filename #!key (css-filename #f) (css-string #f))
    (sxml->html-string
      (pathname-replace-extension input-filename "html")
      (md->sxml input-filename css-filename: css-filename css-string: css-string)))

  ;;;
  ;;; Index files
  ;;;

  (define (ent-func dir tag fname . rest)
    (assert (eq? tag 'ent))
    (let* ((href (make-pathname dir (pathname-replace-extension fname "html"))))
      `(a (@ (href ,href)) ,(string-intersperse `(,fname ,@rest) "\t"))))

  (define (dir-func tag dir . ents)
    (assert (eq? tag 'dir))
    `(,(string-append "\n" dir ":\n")
       ,@(append (intersperse (map (lambda (ent) (apply ent-func `(,dir ,@ent))) ents) "\n"))))

  (define (idx-func #!key (css-filename #f) (css-string #f))
    (let ((css (or css-string
                   (and css-filename
                        (file-exists? css-filename)
                        (read-to-string css-filename)))))
      (lambda (tag title . dirs)
        (assert (eq? tag 'idx))
        `(page ,title ,css
               (h1 ,title)
               ,(concatenate (intersperse (map (cut apply dir-func <>) dirs) '("\n")))))))

  (define (idx->html idx-file #!key (css-filename #f) (css-string #f))
    (let* ((html (apply (idx-func css-filename: css-filename css-string: css-string) (read-to-list idx-file)))
           (out-file (pathname-replace-extension idx-file "html")))
      (sxml->html-string out-file html))))
