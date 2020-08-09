#lang racket

(provide tmp unique-variable unique-box)

(define tmp
  (let ()
    (define i 0)
    (lambda ()
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append "tmp" (number->string n)))))))

(define unique-variable
  (let ()
    (define i 0)
    (lambda (x)
      (let ([n i])
        (set! i (add1 i))
        (string->symbol
          (string-append (symbol->string x) "." (number->string n)))))))

(define unique-box
  (let()
    (define i 0)
    (define (ni)
      (let ([n i])
        (set! i (add1 i))
        n))

    (define (internal-ubox sym)
      (string->symbol
        (string-append
          (symbol->string sym)
          ".ubox"
          (number->string (ni)))))

      (lambda (sym)
        (let ([char* (string->list (symbol->string sym))])
          (let loop ([seendot #f] [rchar* (reverse char*)] [suffix '()])
            (cond
              [(null? rchar*) (internal-ubox sym)]
              [seendot
               (string->symbol
                 (string-append
                   (list->string (reverse rchar*))
                   ".box"
                   (list->string suffix)))]
              [(eq? #\. (car rchar*))
               (loop #t (cdr rchar*) suffix)]
              [(char-numeric? (car rchar*))
               (loop #f (cdr rchar*) (cons (car rchar*) suffix))]
              [else (internal-ubox sym)]))))))

(module+ test
  (require rackunit)

  (check-eq? (unique-box 'a.20) 'a.box20)
  (check-eq? (unique-box '.a.25) '.a.box25)
  (check-eq? (unique-box 'a.b.30) 'a.b.box30)
  (check-eq? (unique-box '.a.b.35) '.a.b.box35)

  (check-eq? (unique-box 'a) 'a.ubox0)
  (check-eq? (unique-box 'a.b) 'a.b.ubox1)
  (check-eq? (unique-box '.a) '.a.ubox2)
  )
