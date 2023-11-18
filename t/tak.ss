(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "tak"
      (test-case
        (letrec ([tak (lambda (x y z)
                        (if (not (< y x))
                          z
                          (tak (tak (sub1 x) y z)
                               (tak (sub1 y) z x)
                               (tak (sub1 z) x y))))])
          (tak 18 12 6))
        "7")
      (test-case
        (letrec ([tak (lambda (x y z)
                        (if (not (< y x))
                          z
                          (tak (tak (sub1 x) y z)
                               (tak (sub1 y) z x)
                               (tak (sub1 z) x y))))])
          (tak 18 12 0))
        "1")
      (test-case
        (letrec ([tak (lambda (x y z)
                        (if (not (< y x))
                          z
                          (tak (tak (sub1 x) y z)
                               (tak (sub1 y) z x)
                               (tak (sub1 z) x y))))])
          (tak 20 12 0))
        "1")
)))
