(ns com.joejag.99problems.21-30
  (:use clojure.test))

; P21 (*) Insert an element at a given position into a list.
; * (my-insert-at 'alfa '(a b c d) 2)
;   (A ALFA B C D)

(defn my-insert-at [item coll pos]
  (let [split-list (split-at (dec pos) coll)]
  (concat (first split-list) (list item) (second split-list))))

; TESTS

(deftest test-my-insert-at
  (is (= '(a "alfa" b c d) (my-insert-at "alfa" '(a b c d) 2))))


(run-tests)