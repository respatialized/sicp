#lang sicp

(define ex1.1-initial-answers
  '(10 12 8 3 6 a b 19 false 4 16 6 16))

(define ex1.1-correct?
  '(true true true true true false false true true true true true true))

;; what I got wrong: the REPL doesn't print the name of an assigned variable.

(define ex1.2 (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
                 (* 3 (- 6 2) (- 2 7))))

(define (ex1.3 x y z) (cond
                        ((and (> x z) (> y z)) (+ (* x x) (* y y)))
                        ((and (> x y) (> z y)) (+ (* x x) (* z z)))
                        ((and (> z x) (> y x)) (+ (* y y) (* z z)))))

(define ex1.4 "the outermost expression uses an operator whose value is determined by the conditional expression - if b is less than zero, it's subtracted from a, which is equivalent to adding its absolute value. if it's zero or greater, it's added to a.")

(define ex1.5 "with applicative order of evaluation, the call stack blows up because the program never terminates. fully expanding the values passed INTO the conditional if means calling p - and calling p calls p which calls p and so on ad infinitum. normal order evaluation avoids this because the definition of (test ...) is expanded to the conditional expression BEFORE p gets called, so because the first part of the conditional evaluates to true, p never gets called.")


(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x) (< (abs (- (* guess guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define ex1.6-initial "it works, because the else expression only encounters one condition before, so whether it gets evaluated depends just on the truth of the first predicate.")

(define ex1.6-actual "the program never terminates, because the else returns an unevalutated expression, which just expands the form recursively forever.")

(define ex1.7-examples '((* (sqrt 0.0000025) (sqrt 0.0000025))
                         (* (sqrt 124444444444400000000000) (sqrt 124444444444400000000000))))

(define ex1.7-explanation "the first example fails because it yields 0.000978 for the square root of 0.0000025 - so it's off by a couple of orders of magnitude. the second example doesn't terminate, which I think is because the integers given are so big that the program can't keep track of the mantissa and therefore can't determine whether the number passes the (good-enough?) threshold.")

(define (good-enough-frac? guess x)
  (< (abs (- 1 (/ x (* guess guess)))) 0.001))

(define (ex1.7 guess x) (if (good-enough-frac? guess x) guess
                            (ex1.7 (improve guess x) x)))
(define (sqrt-1.7 x) (ex1.7 1.0 x))

(define ex1.7-examples-2 '((sqrt-1.7 0.0000025) (sqrt-1.7 124444444444400000000000)))
;; it indeed works better.


(define (improve-cube guess x) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
(define (good-enough-cube-frac? guess x)
  (< (abs (- 1 (/ x (* guess guess guess)))) 0.001))
(define (ex1.8 guess x)
  (if (good-enough-cube-frac? guess x) guess
      (ex1.8 (improve-cube guess x) x)))

(define ex1.9 "the first is recursive, because it has to fully expand the successive list of (inc) calls before reducing to the final sum. the second is linear iterative - each stage only has one call to (+), and the reduction happens before the call.")


(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define ex1.10-examples '(1024 65536 65536))
(define ex1.10-closedform '("2n" "2^N" "2^(2^N)"))


(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (ex1.10-recursive n)
  (if (< n 3) n
      (+ (ex1.10-recursive (- n 1))
         (* 2 (ex1.10-recursive (- n 2)))
         (* 3 (ex1.10-recursive (- n 3))))))

(define (fib n)
  (define (iter a b count)
    (if (= count 0) b
        (iter b (+ a b) (- count 1))))
  (iter 0 1 n))


(define (ex1.10-iterative n)
  ;; to do it iteratively, you have to hold on to three pieces of data.
  (define (iter a b c count)
    (if (= count 0) c
        (iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (if (<= n 3) n
      (iter 1 2 3 (- n 3))))

(define (conj i l)
  (reverse (cons i (reverse l))))

(define (ex1.12 n)
  (define (build-row-iter items acc)
    (let ((h (car items))
          (rest (cdr items))
          (t (car (cdr items))))

      (if ;; base case: last element is one
       (= 1 t)
       (conj 1 (conj (+ h t) acc))
       (build-row-iter rest (conj (+ h t) acc)))))

  (define (next-row row)
    (if (= (length row) 1) '(1 1)
        (build-row-iter row '(1))))

  (define (iter row count)
    (if (= n count) row
        (iter (next-row row) (+ count 1))))
  (if (= n 1) '(1)
      (iter '(1) 0)))
