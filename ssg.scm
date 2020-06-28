#;(
(import chicken.base)
(import chicken.file)
(import chicken.file.posix)
(import chicken.irregex)
(import chicken.pathname)
(import chicken.process-context)
(import optimism)
(import srfi-1)
(import srfi-13)
(import typed-records)

(import md2html)
(import fold-args)

(define-constant *DEPTH-OPTS*   '(-d --depth))
(define-constant *DIR-OPTS*     '(-D --directory))
(define-constant *DO-IT-OPT*    '--do-it)
(define-constant *HELP-OPTS*    '(-h --help))
(define-constant *IDX-OPTS*     '(|-i| --index)) ; |-i| because chicken reads -i as a complex number
(define-constant *STYLE-OPTS*   '(-s --style))
(define-constant *VERBOSE-OPTS* '(-v --verbose))
(define-constant *OPTS*
  `(; `-d DEPTH`: max depth to search for files
    (,*DEPTH-OPTS* . depth)

    ; `-D DIRECTORY`: search for files in DIRECTORY
    (,*DIR-OPTS* . dir)

    ; `--do-it`: DO IT!
    (,*DO-IT-OPT*)

    ; `-h`: Show help
    (,*HELP-OPTS*)

    ; `-i INDEX-FILE`: Create an HTML file from an index file
    (,*IDX-OPTS* . idx-file)

    ; `-s`: Embed css-file in generated HTML
    (,*STYLE-OPTS* . css-file)

    ; `-v`: show a message on each file it processes
    (,*VERBOSE-OPTS*)))

(defstruct options depth dirs files do-it help style verbose idx)

(define (get-opts args)
  (define (proc-file? fname ext)
    (let ((html (pathname-replace-extension fname "html")))
      (and (string-suffix? ext fname)
	   (file-exists? fname)
	   (or (not (file-exists? html))
	       (> (file-modification-time fname)
		  (file-modification-time html))))))

  (define (get-files options)
    (filter
      (cute proc-file? <> ".md")
      (append (options-files options)
	      (append-map
		(lambda (dir)
		  (find-files dir
			      #:limit (options-depth options)
			      #:test (irregex ".*\\.md$")))
		(options-dirs options)))))

  (define (get-idx options)
    (filter (cute proc-file? <> ".scm") (options-idx options)))

  (define (kons ret arg)
    (let ((opt (car arg)))
      (cond
	((memq opt *DEPTH-OPTS*)
	 (update-options ret #:depth (string->number (cdr arg))))
	((memq opt *DIR-OPTS*)
	 (update-options ret #:dirs (cons (cdr arg) (options-dirs ret))))
	((memq opt *STYLE-OPTS*)
	 (update-options ret #:style (cdr arg)))
	((memq opt *HELP-OPTS*)
	 (update-options ret #:help #t))
	((memq opt *IDX-OPTS*)
	 (update-options ret #:idx (cons (cdr arg) (options-idx ret))))
	((memq opt *VERBOSE-OPTS*)
	 (update-options ret #:verbose #t))
	((eq? *DO-IT-OPT* opt)
	 (update-options ret #:do-it #t))
	((eq? '-- opt)
	 (update-options ret #:files (cdr arg))))))

  (define (parse-args args)
    (fold-args kons
	       (make-options #:depth #f #:help    #f #:do-it #f
			     #:style #f #:verbose #f
			     #:dirs '() #:files '() #:idx '())
	       *OPTS* args))

  (let* ((options (parse-args args))
	 (options (update-options
		    options #:dirs
		    (if (and (null? (options-dirs options))
			     (null? (options-files options))
			     (null? (options-idx options)))
			'(".")
			(filter directory-exists?
				(options-dirs options))))))
    (update-options options
		    #:files (get-files options)
		    #:idx (get-idx options))))

(define (do-md->html fname verbose css-string)
  (when verbose (print "MD -> HTML:\t" fname))
  (md->html fname #:css-string css-string))

(define (do-idx->html fname verbose css-string)
  (when verbose (print "Index -> HTML:\t" fname))
  (idx->html fname #:css-string css-string))

(define (help)
  (print
    (program-name) " [OPTION...] [--] [FILE...]\n"
    "   -h --help                  show this help message\n"
    "   -d --depth DEPTH           max directory recursion depth\n"
    "   -D --directory DIRECTORY   search for files in this directory\n"
    "   -i --index INDEX-FILE      create an HTML file from an index file\n"
    "   -s --style CSS-FILE        embed a CSS file in the generated HTML\n"
    "   -v --verbose               print each filename before processing\n"
    "      --do-it                 actually do things"))

(define (usage)
  (print (program-name) " [-d DEPTH] [-D DIR]... [--do-it] [--] [FILE]..."))

(define (main args)
  (let* ((options (get-opts args))
	 (files (options-files options))
	 (idxs (options-idx options)))
    (cond
      ((options-help options)
       (help))
      ((and (null? files) (null? idxs))
       (usage))
      ((options-do-it options)
       (let* ((css (options-style options))
	      (css (and css (read-to-string css))))
	 (for-each (cut do-md->html <> (options-verbose options) css) files)
	 (for-each (cut do-idx->html <> (options-verbose options) css) idxs)))
      (else
	(print
	  "MD: " files "\n"
	  "Index: " idxs "\n"
	  "Use `" *DO-IT-OPT* "` to process the files above")))))

(main (command-line-arguments))
)

(module
  ssg
  *

  (import scheme chicken.base)

  (import
    ssg.result
    ssg.site
    )

  (define (ssg site)
    (let ((site
	    (result-case
	      site
	      (#:ok (site) site)
	      (#:error (reason) (error reason)))))
      (result-error 'TODO)))
  )
