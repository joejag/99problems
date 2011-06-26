(ns com.joejag.99.10-20
  (:use clojure.test))

;P11 (*) Modified run-length encoding.
;Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result
; list. Only elements with duplicates are transferred as (N E) lists.
; (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode [coll] ; copied from P10
  (map #((juxt count first) %) (partition-by identity coll)))

(defn encode-modified [coll]
  (map #(cond (= (first %) 1) (second %) :else %) (encode coll)))

; P12 (**) Decode a run-length encoded list.
; Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
; (decode '((4 A) B (2 C) (2 A) D (4 E)))
; (a a a a b c c a a d e e e e)

(defn decode [coll]
  (flatten (map #(cond (seq? %) (repeat (first %) (second %)) :else %) coll)))

; P13 (**) Run-length encoding of a list (direct solution).
; Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists
; containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by
; replacing the singleton lists (1 X) by X.
; (encode-direct '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

(defn- sublist [& items] list (list items))

(defn encode-direct ([coll] (encode-direct coll '()))
  ([coll result]
    (let [current-letter (first coll)]
      (cond
        ; finished, use same method as P11 to remove singleton lists
        (empty? coll)
        (map #(cond (= (first %) 1) (second %) :else %) result)
        ; start
        (empty? result)
        (recur (rest coll), (sublist 1 current-letter))
        ; same element in input list at end of result list
        (= current-letter (second (last result)))
        (recur (rest coll), (concat (drop-last result) (sublist (inc (first (last result))), current-letter)))
        ; next sublist to be made
        :else
        (recur (rest coll), (concat result (sublist 1 current-letter)))
        )))
  )

;P14 (*) Duplicate the elements of a list.
; (dupli '(a b c c d))
; (A A B B C C C C D D)

(defn dupl [coll]
  (flatten (map #(repeat 2 %) coll)))

; TESTS
(deftest test-encode-modified
  (is (= '((4 a) b (2 c) (2 a) d (4 e)) (encode-modified '(a a a a b c c a a d e e e e)))))

(deftest test-decode
  (is (= '(a a a a b c c a a d e e e e)) (decode '((4 A) B (2 C) (2 A) D (4 E)))))

(deftest test-encode-direct
  (is (= '((4 a) b (2 c) (2 a) d (4 e)) (encode-direct '(a a a a b c c a a d e e e e)))))

(deftest test-dupli
  (is (= '(a a b b c c c c d d) (dupl '(a b c c d)))))

(run-tests)