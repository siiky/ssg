(import chicken.base)
(import chicken.file)
(import chicken.irregex)
(import chicken.pathname)
(import chicken.process-context)
(import optimism)
(import srfi-1)
(import srfi-13)
(import typed-records)

(import md2html)

(define-constant *HELP-OPTS*  '(-h --help))
(define-constant *DEPTH-OPTS* '(-d --depth))
(define-constant *DIR-OPTS*   '(-D --directory))
(define-constant *DO-IT-OPT* '--do-it)
(define-constant *OPTS*
                 `(; `-d DEPTH`: max depth to search for files
                   (,*DEPTH-OPTS* depth)

                   ; `-D DIRECTORY`: search for files in DIRECTORY
                   (,*DIR-OPTS* dir)

                   ; `--do-it`: DO IT!
                   (,*DO-IT-OPT*)

                   ; `-h`: Show help
                   (,*HELP-OPTS*)))

(defstruct options depth dirs files do-it help)

(define (get-opts args)
  (define (get-files options)
    (filter (lambda (fname)
              (and (string-suffix? ".md" fname)
                   (file-exists? fname)))
            (append (options-files options)
                    (append-map
                      (lambda (dir)
                        (find-files dir
                                    #:limit (options-depth options)
                                    #:test (irregex ".*\\.md$")))
                      (options-dirs options)))))

  (define (kons arg ret)
    (let ((opt (car arg)))
      (cond
        ((memq opt *HELP-OPTS*)
         (update-options ret #:help #t))
        ((memq opt *DIR-OPTS*)
         (update-options ret #:dirs (cons (cadr arg) (options-dirs ret))))
        ((memq opt *DEPTH-OPTS*)
         (update-options ret #:depth (string->number (cadr arg))))
        ((eq? *DO-IT-OPT* opt)
         (update-options ret #:do-it #t))
        ((eq? '-- opt)
         (update-options ret #:files (cdr arg))))))

  (define (parse-args args)
    (fold kons
          (make-options #:depth #f #:dirs  '() #:files '()
                        #:help  #f #:do-it #f)
          (parse-command-line args *OPTS*)))

  (let* ((options (parse-args args))
         (options (update-options
                    options #:dirs
                    (if (and (null? (options-dirs options))
                             (null? (options-files options)))
                        '(".")
                        (filter directory-exists?
                                (options-dirs options))))))
    (update-options options #:files (get-files options))))

(define (do-it! fname)
  (let ((outfname (pathname-replace-extension fname "html"))
        (output (md->html fname)))
    (print "###" fname " -> " outfname ":\n"
           (cdr output)
           "\n==="
           (car output)
           "\n###")))

(define (help)
  (print
    (program-name) " [OPTION...] [--] [FILE...]\n"
    "   -h --help                  show this help message\n"
    "   -d --depth DEPTH           max directory recursion depth\n"
    "   -D --directory DIRECTORY   search for files in this directory\n"
    "      --do-it                 actually do things"))

(define (usage)
  (print (program-name) " [-d DEPTH] [-D DIR]... [--do-it] [--] [FILE]..."))

(define (main args)
  (let* ((options (get-opts args))
         (files (options-files options)))
    (cond
      ((options-help options)
       (help))
      ((null? files)
       (usage))
      ((options-do-it options)
       (for-each do-it! files))
      (else
        (print
          files "\n"
          "Use `" *DO-IT-OPT* "` to process the files above")))))

(main (command-line-arguments))
