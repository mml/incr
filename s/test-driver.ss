#lang racket

(require "compile-shared.ss")
(require "compiler.ss")
(require "config.ss")
(require racket/trace)

(provide test-cases)
(provide test-case)
(provide /test-case)
(provide skip-test-case)
(provide output-dir)
(provide precompiled-driver-object)

; Racketisms
(define system-successful? system)

(define output-dir
  (make-parameter "out"))

(define assembly-file
  (make-parameter "x-test-program.s"))

(define precompiled-driver-object (make-parameter #f))

(define (assembly-path) (string-append (output-dir) "/" (assembly-file)))
(define (object-path) (string-replace (assembly-path) ".s" ".o"))
(define (program-path) (string-replace (assembly-path) ".s" ""))
(define (output-path) (string-replace (assembly-path) ".s" ".out"))

(define (run-compile expr*)
  (let ([p (open-output-file (assembly-path) #:exists 'replace)])
    (parameterize ([compile-port p]
                   [scramble-link-register? #f])
      (compile-program expr*))
    (flush-output p)
    (close-output-port p)))

(define (assemble)
  (let ([cmd (format "~a -g -o ~a ~a" assembler-path (object-path) (assembly-path))])
    #;(printf "~a~n" cmd)
    (unless (system-successful? cmd)
      (error 'as "assemble error"))))

(define (build-driver)
  (unless (system-successful? (format "~a -DNO_NEWLINE -g -o ~a -c ../c/driver.c" c-compiler-path (string-append (output-dir) "/driver.o")))
    (error 'gcc "build error")))

(define (build)
  (unless
    (system-successful?
      (format
        "~a -DNO_NEWLINE -static -g -o ~a ~a ~a"
        c-compiler-path
        (program-path)
        (if (not (precompiled-driver-object))
          (string-append (output-dir) "/driver.o")
          (precompiled-driver-object))
        (object-path)))
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
      ; Pattern matches variable args: (test-case expr0 expr ... expected)
      [(_ desc (test-case expr0 expr ... expected) ...)
       #'(begin
           (when (not (precompiled-driver-object)) (build-driver))
           (/test-cases desc
                        (list (list (quote expr0) (quote expr) ...) ...)
                        (list expected ...)
                        #f))]
      [(_ skip desc (test-case expr0 expr ... expected) ...)
       #'(/test-cases desc
                     (list (list (quote expr0) (quote expr) ...) ...)
                     (list expected ...)
                     #t)])))

(define-syntax (test-case stx)
  (syntax-case stx (str)
    ; String case: (test-case str expr0 expr ... expected)
    [(_ str expr0 expr ... expected)
     (let ([exprs-stx (syntax->list #'(expr0 expr ...))])
       #`(/test-case (list #,@(map (lambda (e) #`(quote #,e)) exprs-stx))
                     (string-append "\"" expected "\"")))]
    ; Normal case: (test-case expr0 expr ... expected)
    [(_ expr0 expr ... expected)
     (let ([exprs-stx (syntax->list #'(expr0 expr ...))])
       #`(/test-case (list #,@(map (lambda (e) #`(quote #,e)) exprs-stx))
                    expected))]))

(define (/test-cases desc expr-lists expecteds skip?)
  (cond
    [skip? (printf "Skipping cases ~s~n" desc)]
    [else
      (printf "Cases '~a'~n" desc)
      (let ([t0 (current-milliseconds)])
        (let f ([expr-lists expr-lists] [expecteds expecteds])
          (cond
            [(null? expr-lists)
             (printf "~vs elapsed for ~s~n"
                     (/ (- (current-milliseconds) t0) 1000.0) desc)]
            [(/test-case (car expr-lists) (car expecteds))
             (f (cdr expr-lists) (cdr expecteds))])))]))

(define (/test-case expr* expected)
  (printf "Test: ~a~n" (pretty-format expr*))
  (run-compile expr*)
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
