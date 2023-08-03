(require "test-driver.ss")

(let f ([i 0])
  (cond
    [(<= i (sub1 (expt 2 29))) (/test-case i (number->string i))
                               (f (add1 i))]
    [else (void)]))
