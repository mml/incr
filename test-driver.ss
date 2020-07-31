#lang racket

(require "compiler.ss")

(provide test-cases)
(provide test-case)
(provide skip-test-case)

; Racketisms
(define system-successful? system)

(define output-file
  (make-parameter
    "x-test-program.s"
    string?))

(define (object-file) (string-replace (output-file) ".s" ".o"))

(define (run-compile expr)
  (let ([p (open-output-file (output-file) #:exists 'replace)])
    (parameterize ([compile-port p]
                   [scramble-link-register? #f])
      (compile-program expr))
    (flush-output p)))

(define (assemble)
  (unless (system-successful? (format "as -g -o ~a ~a" (object-file) (output-file)))
    (error 'as "assemble error")))

(define (build)
  (unless (system-successful? (format "gcc -DNO_NEWLINE -g -o x-test-program driver.c ~a" (object-file)))
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

(define-syntax test-cases
  (lambda (stx)
    (syntax-case stx (skip test-case)
      [(_ desc (test-case expr expected) ...)
       #'(/test-cases desc (list (quote expr) ...) (list expected ...) #f)]
      [(_ skip desc (test-case expr expected) ...)
       #'(/test-cases desc (list (quote expr) ...) (list expected ...) #t)])))

(define-syntax (test-case stx)
  (syntax-case stx ()
    [(_ expr expected)
     (syntax (/test-case (quote expr) expected))]))

(define (/test-cases desc exprs expecteds skip?)
  (cond
    [skip? (printf "Skipping cases ~s~n" desc)]
    [else
      (printf "Cases '~a'~n" desc)
      (let f ([exprs exprs] [expecteds expecteds])
        (cond
          [(null? exprs) (void)]
          [(/test-case (car exprs) (car expecteds))
           (f (cdr exprs) (cdr expecteds))]))]))

(define (/test-case expr expected)
  (printf "Test: ~a~n" (pretty-format expr))
  (run-compile expr)
  (assemble)
  (build)
  (execute)
  (unless (string=? expected (get-string))
    (error 'test "expected ~s got ~s" expected (get-string))))

(define-syntax (skip-test-case stx)
  (syntax-case stx ()
    [(_ expr expected)
     (syntax (/skip-test-case (quote expr)))]))

(define (/skip-test-case expr)
  (printf "SKIP ~a~n" (pretty-format expr)))
