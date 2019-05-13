(module
  fold-args
  (fold-args)
  (import scheme)
  (import chicken.base)
  (import chicken.process-context)
  (import optimism)

  (define (fold-args kons knil grammar
                     #!optional (args (command-line-arguments)))
    (foldl kons knil (parse-command-line args grammar))))
