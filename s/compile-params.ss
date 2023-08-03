#lang racket

(provide compile-port)
(provide scramble-link-register?)

(define scramble-link-register?
  (make-parameter #f))

(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format "Not an output port ~s." p)))
     p)))
