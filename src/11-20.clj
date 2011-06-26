(ns com.joejag.99problems.10-20
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

;P15 (**) Replicate the elements of a list a given number of times.
; (repli '(a b c) 3)
;(A A A B B B C C C)

(defn repli [coll times]
  (flatten (map #(repeat times %) coll)))

;P16 (**) Drop every N'th element from a list.
;* (my-drop-nth '(a b c d e f g h i k) 3)
;(A B D E G H K)

(defn my-drop-nth [coll step]
  (->>
    (map #(cond (= %2 step) nil :else %1) coll (cycle (range 1 (inc step))))
    (remove nil?))
  )

;P17 (*) Split a list into two parts; the length of the first part is given.
;Do not use any predefined predicates.
; (my-split '(a b c d e f g h i k) 3)
;( (A B C) (D E F G H I K))

(defn my-split [coll at] (split-at at coll))

;P18 (**) Extract a slice from a list.
;Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the
; original list (both limits included). Start counting the elements with 1.
; (slice '(a b c d e f g h i k) 3 7)
; (C D E F G)

(defn my-slice [coll i k]
  (->>
    (map #(cond (or (> i %2) (< k %2)) nil :else %1) coll (range 1 (inc (count coll))))
    (remove nil?))
  )


;P19 (**) Rotate a list N places to the left.
;* (my-rotate '(a b c d e f g h) 3)
;  (D E F G H A B C)
;* (my-rotate '(a b c d e f g h) -2)
;  (G H A B C D E F)
;
; Hint: Use the predefined functions length and append, as well as the result of problem P17.

(defn my-rotate [coll places]
  (let [split-list (my-split coll (cond (neg? places) (+ (count coll) places) :else places))]
    (concat (second split-list) (first split-list))
    ))

;P20 (*) Remove the K'th element from a list.
; * (remove-at '(a b c d) 2)
;   (A C D)
(defn remove-at [coll pos]
  (->>
    (map #(cond (= %2 pos) nil :else %1) coll (range 1 (inc (count coll))))
    (remove nil?)
  ))

; TESTS
(deftest test-encode-modified
  (is (= '((4 a) b (2 c) (2 a) d (4 e)) (encode-modified '(a a a a b c c a a d e e e e)))))

(deftest test-decode
  (is (= '(a a a a b c c a a d e e e e)) (decode '((4 A) B (2 C) (2 A) D (4 E)))))

(deftest test-encode-direct
  (is (= '((4 a) b (2 c) (2 a) d (4 e)) (encode-direct '(a a a a b c c a a d e e e e)))))

(deftest test-dupli
  (is (= '(a a b b c c c c d d) (dupl '(a b c c d)))))

(deftest test-repli
  (is (= '(a a a b b b c c c) (repli '(a b c) 3))))

(deftest test-my-drop-nth
  (is (= '(a b d e g h k) (my-drop-nth '(a b c d e f g h i k) 3))))

(deftest test-my-split
  (is (= '((a b c) (d e f g h i k)) (my-split '(a b c d e f g h i k) 3))))

(deftest test-my-slice
  (is (= '(c d e f g) (my-slice '(a b c d e f g h i k) 3 7))))

(deftest test-my-rotate
  (is (= '(d e f g h a b c) (my-rotate '(a b c d e f g h) 3)))
  (is (= '(g h a b c d e f) (my-rotate '(a b c d e f g h) -2))))

(deftest test-remove-at
  (is (= '(a c d) (remove-at '(a b c d) 2))))

(run-tests)