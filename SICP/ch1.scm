(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(define (abs1 x)
  (cond ((> x 0) x)
        (else (- x))))
(define (>= x y) (or (> x y) (= x y)))
(define (>=1 x y) (not (< x y)))
;;1.2
(define (f)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
     (* 3 (- 6 2) (- 2 7))))
;;1.3
(define (f1 x y z)
  (- (+ (square x) (square y) (square z)) (square (min x y z))))
(f1 1 2 3)
;;;------
(define (sqrt-tier guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-tier (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-tier 1.0 x))

(sqrt 9)

;;1.6
;;function parameters would be evaluated before their passed in, thus else-clause would be evaluated first which causes an infinity recursion
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;1.8
(define (func x y)
  (/(+ (/ x (square y)) (* 2 y)) 3))
(define (cube x) (* x x x))
(define (next guess x) (func x guess))
(define (good? guess x)
  (< (abs (- x (cube guess))) 0.001))
(define (cube-root-tier guess x)
  (if (good? guess x)
      guess
      (cube-root-tier (next guess x) x)))
(define (cube-root x)
  (cube-root-tier 1.0 x))

