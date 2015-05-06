
#lang scheme

(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))

(define the-empty-stream (delay '()))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (empty-stream? stream)
  (if (eq? (stream-car stream) the-empty-stream)
      #t
      #f)
  )
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (show x)
  (display x)
  x)
  
(define (stream-enumerate-interval start end)
  (cond ((> start end) the-empty-stream)
        (else (cons-stream start (stream-enumerate-interval (+ start 1) end))
        )))

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)