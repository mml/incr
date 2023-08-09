(module tests racket
  (provide runtests)
  (require "../s/test-driver.ss")
  (define (runtests)
    (test-cases "vectors"
      (test-case (make-vector 0 0) "#()")
      (test-case (make-vector 1 0) "#(0)")
      (test-case (make-vector 2 1) "#(1 1)")
      (test-case (make-vector 3 2) "#(2 2 2)")
      (test-case (make-vector 10 0) "#(0 0 0 0 0 0 0 0 0 0)")
      (test-case
        (vector-length (make-vector 500 1))
        "500")
      (test-case
        (let ([v (make-vector 5 0)])
          (vector-set! v 1 20)
          v)
        "#(0 20 0 0 0)")
      (test-case
        (let ([v (make-vector 5 0)]
              [u (make-vector 5 10)])
          (vector-set! v 1 (begin
                             (vector-set! u 3 30)
                             20))
          v)
        "#(0 20 0 0 0)")
      (test-case
        (let ([v (make-vector 5 0)])
          (vector-set! v 0 0)
          (vector-set! v 1 1)
          (vector-set! v 2 2)
          (vector-set! v 3 3)
          (vector-set! v 4 4)
          v)
        "#(0 1 2 3 4)")
      (test-case
        (let ([v (make-vector 5 0)])
          (letrec
            ([uv (lambda (v n)
                   (cond
                     [(< n 0) v]
                     [else
                       (vector-set! v n n)
                       (uv v (sub1 n))]))])
            (uv v 4)))
        "#(0 1 2 3 4)")
      (test-case
        (make-vector 40 0)
        "#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)")
      (test-case
        (letrec ([loop (lambda (n v)
                         (if (zero? n)
                           #t
                           (loop (sub1 n) (make-vector 40 n))))])
          (loop 41 #f))
        "#t")
      (test-case
        (let* ([size 40] [v (make-vector size #f)])
          (letrec
            ([init (lambda (n)
                     (cond
                       [(< n 0)]
                       [else
                         (letrec
                           ([vv (make-vector size #f)]
                            [setup (lambda (m)
                                     (cond
                                       [(< m 0) v]
                                       [else
                                         (vector-set! vv m (* m n))
                                         (setup (sub1 m))]))])
                            (vector-set! v n vv)
                            (setup (sub1 size)))
                         (init (sub1 n))]))])
            (init (sub1 size))
            (letrec
              ([check (lambda (m n)
                        (if (< m 0)
                            #t
                            (and (= (* m n) (vector-ref (vector-ref v m) n))
                                 (= (* m n) (vector-ref (vector-ref v n) m))
                                 (check (sub1 m) n))))]
               [check* (lambda (n)
                         (if (< n 0)
                             #t
                             (and (check (sub1 size) n)
                                  (check* (sub1 n)))))])
              (check* (sub1 size)))))
        "#t")

      ; Something that takes long enough to be a speed test.
      (test-case
        (let ([size 10000])
          (letrec
            ([v (make-vector size 1)]
             [f (lambda (n acc)
                  (cond
                    [(< n 0) acc]
                    [else (f (sub1 n) (+ acc (vector-ref v n)))]))]
             [g (lambda (n acc)
                  (cond
                    [(zero? n) acc]
                    [else (g
                            (sub1 n)
                            (+ acc
                               (bitwise-arithmetic-shift-right
                                 (f (sub1 size) 0) 4)))]))])
            (g 10000 0)))
        "6250000")
      )

    ))
