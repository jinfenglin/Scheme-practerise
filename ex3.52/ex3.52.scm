#lang scheme

(define-syntax cons-stream
 (syntax-rules ()
   ((cons-stream a b)
    (cons a (delay b)))))
(define the-empty-stream (delay '()))
(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))
(define stream-car car)
(define (tail s) (force (cdr s)))
(define stream-cdr tail)

(define (stream-map proc s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval start end)
  (if (> start end) 
      the-empty-stream
      (cons-stream start (stream-enumerate-interval (+ 1 start) end ))
      )
  )
(define (even? x)  
    (if (= (/ x 2) 0)
        #t
        #f)
    )
(define (stream-for-each proc s)
  (if (empty-stream? s)
      'done
      (begin (proc (stream-car s))
(stream-for-each proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pre stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pre (stream-car stream)) 
         (cons-stream (stream-car stream) 
                      (stream-map pre (stream-cdr stream))))
        (else (stream-map pre (stream-cdr stream)))))


(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
seq))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
(display-stream y)
(stream-ref y 7)
;(display-stream z)
