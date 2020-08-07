#lang racket

(require "compiler.ss")
(require racket/trace)

(provide test-cases)
(provide test-case)
(provide /test-case)
(provide skip-test-case)

; Racketisms
(define system-successful? system)

(define output-dir
  (make-parameter
    "/dev/shm/incr"
    string?))

(define assembly-file
  (make-parameter
    "x-test-program.s"
    string?))

(define (assembly-path) (string-append (output-dir) "/" (assembly-file)))
(define (object-path) (string-replace (assembly-path) ".s" ".o"))
(define (program-path) (string-replace (assembly-path) ".s" ""))
(define (output-path) (string-replace (assembly-path) ".s" ".out"))

(define (run-compile expr)
  (let ([p (open-output-file (assembly-path) #:exists 'replace)])
    (parameterize ([compile-port p]
                   [scramble-link-register? #f])
      (compile-program expr))
    (flush-output p)
    (close-output-port p)))

(define (assemble)
  (let ([cmd (format "as -g -o ~a ~a" (object-path) (assembly-path))])
    #;(printf "~a~n" cmd)
    (unless (system-successful? cmd)
      (error 'as "assemble error"))))

(define (build-driver)
  (unless (system-successful? (format "gcc -DNO_NEWLINE -g -o ~a -c driver.c" (string-append (output-dir) "/driver.o")))
    (error 'gcc "build error")))

(define (build)
  (unless (system-successful? (format "gcc -DNO_NEWLINE -static -g -o ~a ~a ~a" (program-path) (string-append (output-dir) "/driver.o") (object-path)))
    (error 'gcc "build error")))

(define (execute)
  (unless (system-successful? (format "~a > ~a" (program-path) (output-path)))
    (error 'test "execute error")))

(define (get-string)
  (with-output-to-string
    (lambda ()
      (let ([port (open-input-file (output-path))])
        (let f ()
          (let ([c (read-char port)])
            (cond [(eof-object? c) (close-input-port port)]
                  [else (display c)
                        (f)])))))))

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

(build-driver)
