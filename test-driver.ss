; Racketisms
(define system-successful? system)

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))

(define output-file
  (make-parameter
    "x-test-program.s"
    string?))

(define (object-file) (string-replace (output-file) ".s" ".o"))

(define (run-compile expr)
  (let ([p (open-output-file (output-file) #:exists 'replace)])
    (parameterize ([compile-port p])
      (compile-program expr))
    (flush-output p)))

(define (assemble)
  (unless (system-successful? (format "as -g -o ~a ~a" (object-file) (output-file)))
    (error 'as "assemble error")))

(define (build)
  (unless (system-successful? (format "gcc -DNO_NEWLINE -o x-test-program driver.c ~a" (object-file)))
    (error 'gcc "build error")))

(define (execute)
  (unless (system-successful? "./x-test-program > x-test-program.out")
    (error 'test "execute error")))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (with-input-from-file
	"x-test-program.out"
	(lambda ()
	  (let f ()
	    (let ([c (read-char)])
	      (cond
		[(eof-object? c) (void)]
		[else (display c)
		      (f)]))))))))

(define (test-case expr expected)
  (printf "Test: ~a~n" (pretty-format expr))
  (run-compile expr)
  (assemble)
  (build)
  (execute)
  (unless (string=? expected (get-string))
    (error 'test "expected ~s got ~s" expected (get-string))))

(define (emit . args)
  (apply fprintf (compile-port) args)
  (newline (compile-port)))

