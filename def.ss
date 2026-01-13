(display (let ([y (lambda () 5)])
  (define (x) (y))
  (define (y) 10)
  (+ (x) (y))))
