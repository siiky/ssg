# `ssg`

Example site program:

```scm
#!/usr/bin/env -S csi -s

(import
  (only chicken.process-context command-line-arguments)

  (only srfi-1 member)

  (prefix
    (only ssg
          ssg)
    |ssg:|)

  (prefix
    (only ssg.css
          css-content
          css-file)
    |ssg:|)

  (prefix
    (only ssg.index
          dir
          ent
          idx)
    |ssg:|)

  (prefix
    (only ssg.converters.lowdown
          make-sxml-custom-rules
          idx->html
          md->html)
    |ssg:lowdown:|)

  (prefix
    (only ssg.converters.pandoc
          md->html)
    |ssg:pandoc:|)

  (prefix
    (only ssg.feed
          feed-options)
    |ssg:|)

  (prefix
    (only ssg.site
          make-converter-table
          site)
    |ssg:|)
  )

(define-constant feed-output-path "atom.xml")

(define index
  (let ((wip 'wip))
    (ssg:idx "Site title"
      (ssg:dir "some_dir"
        (ssg:ent "post1.md" "date" "title")
        (ssg:ent "post2.md" "date" "title"))
      ; Note the wip (it can be placed anywhere inside the dir list)
      (ssg:dir wip "some_wip_dir"
        (ssg:ent "post3.md" "date" "title"))
      (ssg:dir "some_other_dir"
        ; Note the wip (it can be placed anywhere inside the ent list)
        (ssg:ent wip "post4.md" "date" "title")))))

(define converter-table (ssg:make-converter-table ("md" "html" ssg:pandoc:md->html)))
(define css (ssg:css-file "path/to/some/file.css"))
(define index-maker ssg:lowdown:idx->html)

(define feed (ssg:feed-options
               #:authors "me"
               #:id "https://mywebsite.xyz"
               #:path feed-output-path
               #:type 'atom))

(ssg:ssg
  (ssg:site
    #:feed feed
    #:converter-table converter-table
    #:css css
    #:index index
    #:index-maker index-maker
    #:sxml-custom-rules (ssg:lowdown:make-sxml-custom-rules)
    #:force-redo? (member "--force-redo" (command-line-arguments) string=?)))
```

This is just a normal CHICKEN program -- you can run it however you want:
compile it, run it as a script with `csi -s`, or add a shebang and `chmod u+x`.
I use this last option.

Note that there shouldn't be a need to `prefix` or `only` the imports -- I've
done it in this example for clarity. The only two modules that have clashing
identifiers in `ssg` are `ssg.converters.lowdown` and `ssg.converters.pandoc`,
that I've prefixed with `ssg:lowdown:` and `ssg:pandoc:`, respectively.
