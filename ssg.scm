(import chicken.base)
(import chicken.file)
(import chicken.irregex)
(import chicken.pathname)
(import chicken.process-context)
(import optimism)
(import srfi-1)
(import srfi-13)

(import md2html)

(define-constant *HELP-OPT*  '(-h --help))
(define-constant *DEPTH-OPT* '(-d --depth))
(define-constant *DIR-OPT*   '(-D --directory))
(define-constant *DO-IT-OPT* '--do-it)
(define-constant *OPTS*
                 `(; `-d DEPTH`: max depth to search for files
                   (,*DEPTH-OPT* depth)

                   ; `-D DIRECTORY`: search for files in DIRECTORY
                   (,*DIR-OPT* dir)

                   ; `--do-it`: DO IT!
                   (,*DO-IT-OPT*)

                   ; `-h`: Show help
                   (,*HELP-OPT*)))

(: ->bool (* --> boolean))
(define ->bool (compose not not))

(define (do-it! fname)
  (let ((outfname (pathname-replace-extension fname "html"))
        (output (md->html fname)))
    (print "###" fname " -> " outfname ":\n" (cdr output) "\n===" (car output) "\n###")))

(define (help)
  (print
    (program-name) " [OPTION...] [--] [FILE...]\n"
    "   -h --help                  show this help message\n"
    "   -d --depth DEPTH           max directory recursion depth\n"
    "   -D --directory DIRECTORY   search for files in this directory\n"
    "      --do-it                 actually do things\n"))

(define (usage)
  (print (program-name) " [-d DEPTH] [-D DIR]... [--do-it] [--] [FILE]..."))

(define (get-dirs parsed-args)
  (define (dir-opt? arg)
    (->bool (memq (car arg) *DIR-OPT*)))

  (let ((dirs (map cadr (filter dir-opt? parsed-args))))
    (if (null? dirs)
        '(".")
        (filter directory-exists? dirs))))

(define (should-do-it? parsed-args)
  (->bool (assoc *DO-IT-OPT* parsed-args)))

(define (get-files dirs parsed-args depth)
  (filter
    (lambda (fname)
      (and (string-suffix? ".md" fname)
           (file-exists? fname)))
    (append
      (cdr (assoc '-- parsed-args))
      (append-map
        (lambda (dir)
          (find-files dir #:limit depth #:test (irregex ".*\\.md$")))
        dirs))))

(define (get-depth parsed-args)
  (define (depth-opt? arg)
    (->bool (memq (car arg) *DEPTH-OPT*)))
  (fold
    (lambda (arg ret)
      (if (depth-opt? arg)
          (string->number (cadr arg))
          ret))
    #f parsed-args))

(define (show-help? parsed-args)
  (define (help-opt? arg)
    (->bool (memq (car arg) *HELP-OPT*)))
  (any help-opt? parsed-args))

(define (main args)
  (let* ((parsed-args (parse-command-line args *OPTS*))
         (depth (get-depth parsed-args))
         (dirs (get-dirs parsed-args))
         (files (get-files dirs parsed-args depth)))
    (cond
      ((show-help? parsed-args)
       (help))
      ((null? files)
       (usage))
      ((should-do-it? parsed-args)
       (for-each do-it! files))
      (else
        (print files)
        (print "It would be done to the files above")
        (print "Use `" *DO-IT-OPT* "` to execute")))))

(main (command-line-arguments))
