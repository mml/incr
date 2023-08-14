(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "equality, equivalence, etc."
      
    ;(define interesting-numbers '(-1 0 1 999))

    ;Can't do this yet
    #;(for-each (lambda (n)
                (test-case `(= ,n ,n) "#t")))

  (test-case
    (let ((f (lambda () (cons 1 "H"))))
      (eq? (f) (f)))
    "#f")

  (test-case ; from Ghuloum06
    (let ((f (lambda () (quote (1 . "H")))))
      (eq? (f) (f)))
    "#t")

    )))
