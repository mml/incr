(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "equality, equivalence, etc."
      (test-case (= -1 -1) "#t")
      (test-case (= 0 0) "#t")
      (test-case (= 1 1) "#t")
      (test-case (= 999 999) "#t")

      (test-case
    (let ((f (lambda () (cons 1 "H"))))
      (eq? (f) (f)))
    "#f")

  (test-case ; from Ghuloum06
    (let ((f (lambda () (quote (1 . "H")))))
      (eq? (f) (f)))
    "#t")

    )))
