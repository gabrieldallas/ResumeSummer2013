;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bignum) (read-case-sensitive #t) (teachpacks ((lib "cs17.rkt" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "cs17.rkt" "installed-teachpacks")))))
;;This was a project that I worked on with Patrick Loftus
;;The assignment was to implement a way to use lists to represent the natural numbers. We needed to have procedures to convert between ints in racket and our list implemented numbers (called bignums), to add two bignums together, 
;;and to multiply two bignums together in a nontrivial manner (not converting a bignum to an int, adding/multiplying, and converting back). Though we were allowed to, 
;;Patrick and I thought it would be cool if we never used the built in add function to sort of simulate what coding the actual "+" and "*" operations would be like

;;Note: our helper procedure for list reversal is very innefficient; we had not yet learned how to use accumulators. 

;;Bignum: a list of 1s and 0s that represents a number in base 2, and in reverse order
(define-struct bignum (alob))
;;Examples
(make-bignum (list 0 1 1))
(make-bignum (list 1 0 1))

;;The following are helpers for the num-to-bignum

;;find-biggest-power-two: number number --> number
;;I/P: a positive number, num, and the number 0, n, which acts as a counter
(define (find-biggest-power-two num n)
  (cond
    [(> (expt 2 n) num) (- n 1)]
    [(positive? num) (find-biggest-power-two num (+ n 1))]))
;;test cases for find-biggest-power-two
(check-expect (find-biggest-power-two 4 0) 2)
(check-expect (find-biggest-power-two 16 0) 4) 
(check-expect (find-biggest-power-two 789 0) 9)
(check-expect (find-biggest-power-two 1 0) 0)
(check-expect (find-biggest-power-two 0 0) -1)
(check-expect (find-biggest-power-two -1 0) -1)

;;deuces: number --> list
;;I/P: a positive number, num
;;O/P: a list of positive numbers that are all the powers of 2 that add up to num
(define (two-power-list num) 
  (cond
    [(= num 0) empty]
    [(= num 1) (cons 0 empty)]
    [else (cons (find-biggest-power-two num 0) (two-power-list (- num (expt 2 (find-biggest-power-two num 0)))))]))
;;test cases for deuces
(check-expect (two-power-list 5) (list 2 0))
(check-expect (two-power-list 7) (list 2 1 0))
(check-expect (two-power-list 11) (list 3 1 0))
(check-expect (two-power-list 12) (list 3 2)) 
(check-expect (two-power-list 16) (list 4))
(check-expect (two-power-list 8973238535) (list 33 28 26 25 23 22 20 19 15 14 8 2 1 0))

;;binary-list: list --> list
;;I/P: a list, alon, from two-power-list of positive numbers (that are all the powers of 2 that add up to a number); and a counter, counter,
;;which is the largest number from alon
;;O/P: a list of 1s and 0s that represent the num from two-power-list in binary
(define (binary-list alon counter)
  (cond
    [(empty? alon) (cond
                     [(>= counter 0) (cons 0 (binary-list alon (sub1 counter)))]
                     [else empty])]
    [(cons? alon) (cond
                    [(= counter (first alon)) (cons 1 (binary-list (rest alon) (sub1 counter)))]
                    [else (cons 0 (binary-list alon (sub1 counter)))])]))
;;test cases for binary-list
(check-expect (binary-list empty -1) empty)  
(check-expect (binary-list (list 4) 4) (list 1 0 0 0 0))
(check-expect (binary-list (list 2 0) 2) (list 1 0 1 ))
(check-expect (binary-list (list 3 2) 3) (list 1 1 0 0))

;;last-chop: list --> list
;;I/P: a list of 1s and 0s, alob
;;O/P: a list of 1s and 0s without the last number 
(define (last-chop alob)
  (cond
    [(empty? (rest alob)) empty] 
    [(cons? (rest alob)) (cons (first alob) (last-chop (rest alob)))]))
;;test cases for last-chop
(check-expect (last-chop (list 0 1)) (list 0)) 
(check-expect (last-chop (list 1 1 1)) (list 1 1))

;;last: alob --> number
;;I/P: a list of 1s and 0s, alob
;;O/P: the last element in alob before empty
(define (last alob)
  (cond
    [(empty? (rest alob)) (first alob)]
    [(cons? (rest alob)) (last (rest alob))]))
;;test cases for last
(check-expect (last (list 1 0 0 1)) 1)

;;my-reverse: list --> list
;;I/P: a list of 1s and 0s, alob
;;O/P: a list of 1s and 0s that is alob in reverse order
(define (my-reverse alob)
  (cond
    [(empty? alob) empty]
    [(cons? alob) (cons (last alob) (my-reverse (last-chop alob)))]))
;;test cases for my-reverse
(check-expect (my-reverse (list 1 0 1 1 0)) (list 0 1 1 0 1))

;;num-to-bignum: number --> bignum
;;I/P: a natural number
;;O/P: a bignum representation of the input number
(define (num-to-bignum num)
  (my-reverse 
   (binary-list (two-power-list num) (first (two-power-list num)))))
;;test cases for num-to-bignum
(check-expect (num-to-bignum 5) (list 1 0 1))
(check-expect (num-to-bignum 9001) (list 1 0 0 1 0 1 0 0 1 1 0 0 0 1))

;;bignum-to-num-helper: bignum --> number
;;I/P: a bignum, bignum, and a counter, counter, that starts at 0
;;O/P: a number representation of bignum
(define (bignum-to-num-helper bignum counter)
  (cond
    [(empty? bignum) 0]
    [(cons? bignum) 
     (cond
       [(= (first bignum) 0) (+ 0 (bignum-to-num-helper (rest bignum) (+ 1 counter)))]
       [else (+ (expt 2 counter) (bignum-to-num-helper (rest bignum) (+ 1 counter)))])]))
;;test cases for bignum-to-num-helper
(check-expect (bignum-to-num-helper empty 0) 0)
(check-expect (bignum-to-num-helper (list 1 1 1) 0) 7)
(check-expect (bignum-to-num-helper (list 1 0 1) 0) 5)
(check-expect (bignum-to-num-helper (list 0 0 0 1) 0) 8)

;;bignum-to-num: bignum --> number
;;I/P: a bignum, bignum
;;O/P: a number representation of bignum
(define (bignum-to-num bignum)
  (bignum-to-num-helper bignum 0))
;;test cases for bignum-to-num
(check-expect (bignum-to-num (list 1 1 1)) 7)
(check-expect (bignum-to-num (list 1 0 1)) 5)
(check-expect (bignum-to-num (list 0 0 0 1)) 8)

;;The following two procedures are what let us not use the built-in + besides for incrementation

;;add-set a number and a carry value used i adding that number
;;both values will either be a one or a zero
(define-struct add-set (digit carry-value))
;;Examples
(make-add-set 1 0)
(make-add-set 0 0)

;;add-helper-helper: number number number --> add-set
;;I/P: two numbers that are to be added, num1 and num2, and a carry digit, carry
;;O/P: an add-set with the appropriate digit and carry digit, based on the combinations of inputs
(define (add-helper-helper num1 num2 carry)
  (cond
    [(and (= num1 0) (= num2 0) (= carry 0)) (make-add-set 0 0)]
    [(and (= num1 0) (= num2 0) (= carry 1)) (make-add-set 1 0)]
    [(and (= num1 0) (= num2 1) (= carry 0)) (make-add-set 1 0)]
    [(and (= num1 1) (= num2 0) (= carry 0)) (make-add-set 1 0)]
    [(and (= num1 1) (= num2 1) (= carry 0)) (make-add-set 0 1)]
    [(and (= num1 1) (= num2 0) (= carry 1)) (make-add-set 0 1)]
    [(and (= num1 0) (= num2 1) (= carry 1)) (make-add-set 0 1)]
    [(and (= num1 1) (= num2 1) (= carry 1)) (make-add-set 1 1)]))
;;test cases for add-helper-helper
(check-expect (add-helper-helper 0 0 0) (make-add-set 0 0))
(check-expect (add-helper-helper 0 0 1) (make-add-set 1 0))
(check-expect (add-helper-helper 0 1 0) (make-add-set 1 0))
(check-expect (add-helper-helper 1 0 0) (make-add-set 1 0))
(check-expect (add-helper-helper 1 1 0) (make-add-set 0 1))
(check-expect (add-helper-helper 1 0 1) (make-add-set 0 1))
(check-expect (add-helper-helper 0 1 1) (make-add-set 0 1))
(check-expect (add-helper-helper 1 1 1) (make-add-set 1 1))

;;add-helper: bignum bignum number --> bignum
;;I/P: two bignums, bignum1 and bignum2, and a carry value, carry, which should start at 0
;;O/P: a single bignum representing bignum1 added to bignum2
(define (add-helper bignum1 bignum2 carry)
  (cond
    [(and (empty? bignum1) (empty? bignum2)) 
     (cond
       [(= carry 0) empty]
       [(= carry 1) (cons 1 empty)])]
    [(and (empty? bignum1) (cons? bignum2)) (cons (add-set-digit (add-helper-helper 0 (first bignum2) carry)) 
                                            (add-helper empty (rest bignum2) (add-set-carry-value (add-helper-helper 0 (first bignum2) carry))))]
    [(and (cons? bignum1) (empty? bignum2)) (cons (add-set-digit (add-helper-helper (first bignum1) 0 carry)) 
                                            (add-helper (rest bignum1) empty (add-set-carry-value (add-helper-helper (first bignum1) 0 carry))))]
    [(and (cons? bignum1) (cons? bignum2)) (cons (add-set-digit (add-helper-helper (first bignum1) (first bignum2) carry)) 
                                           (add-helper (rest bignum1) (rest bignum2) (add-set-carry-value (add-helper-helper (first bignum1) (first bignum2) carry))))]))

;;add: bignum bignum --> bignum
;;I/P: two bignums, bignum1 and bignum2, to be added
;;O/P: a bignum representing bignum1 added to bignum2
(define (add bignum1 bignum2)
  (add-helper bignum1 bignum2 0))
;;tests
(check-expect (add empty empty) empty)
(check-expect (add (cons 0 empty) (cons 0 empty)) (cons 0 empty)) 
(check-expect (add (cons 0 empty) (cons 1 empty)) (cons 1 empty))
(check-expect (add (cons 1 empty) (cons 1 empty)) (cons 0 (cons 1 empty)))
(check-expect (add (cons 1 (cons 1 empty)) (cons 1 (cons 1 empty))) (cons 0 (cons 1 (cons 1 empty))))
(check-expect (add (cons 0 (cons 1 empty)) (cons 1 (cons 1 empty))) (cons 1 (cons 0 (cons 1 empty))))
(check-expect (add (cons 0 empty) (cons 0 (cons 1 empty))) (cons 0 (cons 1 empty)))
(check-expect (add (list 1 0 1 1 0 1) (list 1 0 0 1 1 1)) (list 0 1 1 0 0 1 1)) 

;;Multiplication

;;cons-zero, conses however many zeros to the end
;;cons-zero: bignum number --> list
;;I/P: a bignum, bignum1, and a counter representing how many zeroes to add to the end of bignum1
;;O/P: a list with counter number of zeroes added to the end of bignum1
(define (cons-zero bignum1 counter)
  (cond 
    [(= 0 counter) bignum1]
    [(> counter 0) (cons 0 (cons-zero bignum1 (- counter 1)))]))
;;test cases for bignum1
(check-expect (cons-zero (list 1 0 1 0) 0) (list 1 0 1 0))
(check-expect (cons-zero (list 1 0 0 1 1) 3) (list 0 0 0 1 0 0 1 1))

;;multiply-helper: bignum bignum number --> bignum
;;I/P: two bignums, bignum1 and bignum2, and a counter, counter, which starts at 0
;;O/P: a bignum representing the two bignums multiplied
(define (multiply-helper bignum1 bignum2 counter)
  (cond 
    [(empty? bignum2) (cons 0 empty)]
    [(cons? bignum2) 
     (cond
       [(= 1 (first bignum2)) (add (cons-zero bignum1 counter) (multiply-helper bignum1 (rest bignum2) (+ counter 1)))]
       [(= 0 (first bignum2)) (multiply-helper bignum1 (rest bignum2) (+ counter 1))])]))
;;test cases for multiply-helper
(check-expect (multiply-helper (list 1 0 0 1) (list 1 0 1) 0) (list 1 0 1 1 0 1))

;;multiply: bignum bignum --> bignum
;;I/P: two bignums, bignum1 and bignum2
;;O/P: a bignum representing bignum1 multiplied by bignum2
(define (multiply bignum1 bignum2)
  (multiply-helper bignum1 bignum2 0))
;;test cases for multiply
(check-expect (multiply (list 1 0 0 1) (list 1 0 1)) (list 1 0 1 1 0 1))

