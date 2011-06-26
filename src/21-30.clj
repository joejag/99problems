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

; TESTS

(deftest test-my-insert-at
  (is (= '(a "alfa" b c d) (my-insert-at "alfa" '(a b c d) 2))))


(deftest test-my-range
  (is (= '(4 5 6 7 8 9)  (my-range 4 9))))

(run-tests)