# `ssg`

Example site index:

```scm
(idx "Site title"
  (dir "some_dir"
    (ent "post1.md" "date" "title")
    (ent "post2.md" "date" "title")
  )
  ; Note the wip symbol (it can be placed anywhere inside the dir list)
  (dir wip "some_wip_dir"
    (ent "post3.md" "date" "title")
  )
  (dir "some_other_dir"
    ; Note the wip symbol (it can be placed anywhere inside the ent list)
    (ent wip "post4.md" "date" "title)
  )
)
```


To generate `foo.html`, save the above in a file named `foo.scm` (or any other
extension other than `.html`) run `ssg -v --do-it -i foo.scm`.
