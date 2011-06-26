(ns com.joejag.99problems.21-30
  (:use clojure.test))

; P21 (*) Insert an element at a given position into a list.
; * (my-insert-at 'alfa '(a b c d) 2)
;   (A ALFA B C D)

(defn my-insert-at [item coll pos]
  (let [split-list (split-at (dec pos) coll)]
  (concat (first split-list) (list item) (second split-list))))

;P22 (*) Create a list containing all integers within a given range.
;If first argument is smaller than second, produce a list in decreasing order.
;* (my-range 4 9)
;   (4 5 6 7 8 9)

(defn my-range [start finish] (range start (inc finish)))

;P23 (**) Extract a given number of randomly selected elements from a list.
;The selected items shall be returned in a list.
;* (rnd-select '(a b c d e f g h) 3)
;   (E D A)
;
;Hint: Use the built-in random number generator and the result of problem P20.
(defn rnd-select [coll amount] (take amount (shuffle coll)))

;P24 (*) Lotto: Draw N different random numbers from the set 1..M.
;The selected numbers shall be returned in a list.
;* (lotto-select 6 49)
;(23 1 17 33 21 37)
;
;Hint: Combine the solutions of problems P22 and P23.

(defn lotto-select [amount size]
  (rnd-select (my-range 1 size) amount)
  )

; TESTS

(deftest test-my-insert-at
  (is (= '(a "alfa" b c d) (my-insert-at "alfa" '(a b c d) 2))))


(deftest test-my-range
  (is (= '(4 5 6 7 8 9)  (my-range 4 9))))

(deftest test-rnd-select
  (is (= 3 (count(rnd-select '(a b c d e f g) 3)))))

(deftest test-lotto-select
  (is (= 6 (count(lotto-select 6 49)))))

; Run tests
(run-tests)