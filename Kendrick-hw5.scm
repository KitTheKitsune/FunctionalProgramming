;Function 1
;convert between inches and centimeters
(define (in->cm x)
  (* x 2.54))

(define (cm->in x)
  (/ x 2.54))

;convert between pounds and kilograms
(define (kg->lb x)
  (* x 2.205))

(define (lb->kg x)
  (/ x 2.205))

;Function 2
;helper for ease of use
(define (square x)
  (* x x))

;Calculate Body Mass Index
(define (BMI-Metric x y)
  (/ y (square (/ x 100))))

(define (BMI-US x y)
  (BMI-Metric (in->cm x) (lb->kg y)))


;Function 3
;CDC classifications of BMI
(define (classify-Metric x y)
  (cond ((< (BMI-Metric x y) 18.5) 'Underweight)
        ((< (BMI-Metric x y) 25.0) 'Normal)
        ((< (BMI-Metric x y) 30.0) 'Overweight)
        (else 'Obese)))

(define (classify-US x y)
  (classify-Metric (in->cm x) (lb->kg y)))

;Function 4
;Counts the amount of positive numbers in a list
(define (num-pos x)
  (if (null? x)
      0
      (if (> (car x) 0)
          (num-pos (cdr x))
          (+ 1 (num-pos (cdr x))))))

;Function 5
;Counts the amount of numbers from a list in a range
(define (num-between x y z)
  (cond ((null? x) 0)
        ((< (car x) y) (num-between (cdr x) y z))
        ((> (car x) z) (num-between (cdr x) y z))
        (else (+ 1 (num-between (cdr x) y z)))))

;Function 6
;Removes the values from a list in a range
(define (remove-between x y z)
  (cond ((null? x) '())
        ((> (car x) z) (cons (car x) (remove-between (cdr x) y z)))
        ((< (car x) y) (cons (car x) (remove-between (cdr x) y z)))
        (else (remove-between (cdr x) y z ))))

;Function 7
;Converts roman numerals (in the form of a list) to arabic numerals
(define (roman->num x)
  (cond ((null? x) 0)
        ((eq? (car x) 'M) (+ 1000 (roman->num (cdr x))))
        ((eq? (car x) 'D) (+ 500 (roman->num (cdr x))))
        ((eq? (car x) 'C) (+ 100 (roman->num (cdr x))))
        ((eq? (car x) 'L) (+ 50 (roman->num (cdr x))))
        ((eq? (car x) 'X) (+ 10 (roman->num (cdr x))))
        ((eq? (car x) 'V) (+ 5 (roman->num (cdr x))))
        ((eq? (car x) 'I) (+ 1 (roman->num (cdr x))))
        (else 'ErrorNotANumeral)))

;Function 8
;converts arabic numerals to roman numerals
(define (num->roman x)
  (cond ((> x 1000) (append '(M) (num->roman (- x 1000))))
        ((> x 500) (append '(D) (num->roman (- x 500))))
        ((> x 100) (append '(C) (num->roman (- x 100))))
        ((> x 50) (append '(L) (num->roman (- x 50))))
        ((> x 10) (append '(X) (num->roman (- x 10))))
        ((> x 5) (append '(V) (num->roman (- x 5))))
        ((>= x 1) (append '(I) (num->roman (- x 1))))
        (else'())))

;Function 9
;helper for ease of use
(define (d6)
  (+ (random 6) 1))

;rolls two dice
(define (dice-roll)
  (+ (d6) (d6)))

;Function 10
;Counts the numer of times y appears in x rolls (without tail-recursion)
(define (count-rolls x y)
  (cond ((<= x 0) 0)
        ((= (dice-roll) y) (+ 1 (count-rolls (- x 1) y)))
        (else (count-rolls (- x 1) y))))

;Function 11
;increments the number at index y
(define (incr x y)
  (cond ((<= y 0) (cons (+ 1 (car x)) (cdr x)))
        (else (cons (car x) (incr (cdr x) (- y 1))))))

;Function 12
;counts the number of all rolls occuring
(define (count-all-rolls x)
  (if (<= x 0)
      '(0 0 0 0 0 0 0 0 0 0 0)
      (incr (count-all-rolls (- x 1)) (- (dice-roll) 2))))
  
  