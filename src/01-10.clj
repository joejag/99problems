(ns com.joejag.99.1-10
   (:use clojure.test))

; P01 (*) Find the last box of a list.
; (my-last '(a b c d))
; (D)

(defn my-last [coll] (last coll))

; P02 (*) Find the last but one box of a list.
; (my-but-last '(a b c d))
;(C D)

(defn my-but-last [coll] (drop (- (count coll) 2) coll))

; P03 (*) Find the K'th element of a list.
; The first element in the list is number 1.
; (element-at '(a b c d e) 3)
; C

(defn element-at [coll idx] (nth coll (dec idx)))

; P04 Find the number of elements of a list.

(defn my-size [coll] (count coll))

; P05 Reverse a list.

(defn my-reverse [coll] (reverse coll))

; P06 Find out whether a list is a palindrome.

(defn palindrome? [coll] (= coll (reverse coll)))

; P07 (**) Flatten a nested list structure.
; (my-flatten '(a (b (c d) e)))
; (A B C D E)

(defn my-flatten [coll] (flatten coll))

; P08 (**) Eliminate consecutive duplicates of list elements.
; If a list contains repeated elements they should be replaced with a single copy of the element.
; The order of the elements should not be changed.
; (my-compress '(a a a a b c c a a d e e e e))
; (A B C A D E)

(defn my-compress [coll]
  (map #(first %) (partition-by identity coll))
  )

; P09 (**) Pack consecutive duplicates of list elements into sublists.
; If a list contains repeated elements they should be placed in separate sublists.
; (my-pack '(a a a a b c c a a d e e e e))
; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defn my-pack [coll]
  (partition-by identity coll))

; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length encoding data compression method.
; Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;  (encode '(a a a a b c c a a d e e e e))
;  ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

(defn encode [coll]
  (map #((juxt count first) %) (my-pack coll)))


; TESTS
(deftest test-my-last
  (is '(d) (my-last '(a b c d))))

(deftest test-my-but-last
  (is '(c d) (my-but-last '(a b c d))))

(deftest test-element-at
  (is 'c (element-at '(a b c d e) 3)))

(deftest test-my-size
  (is 4 (my-size '(a b c d))))

(deftest test-my-reverse
  (is '(d c b a) (my-reverse '(a b c d))))

(deftest test-palindrome?
  (is true (palindrome? "racecar"))
  (is (false? (palindrome? "joe"))))

(deftest test-my-flatten
  (is '(a b c d e) (my-flatten '(a (b (c d) e)))))

(deftest test-my-compress
  (is '(a b c a d e) (my-compress '(a a a a b c c a a d e e e e))))

(deftest test-my-pack
  (is '((a a a a) (b) (c c) (a a) (d) (e e e e)) (my-pack '(a a a a b c c a a d e e e e))))

(deftest test-encode
  (is '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) (encode '(a a a a b c c a a d e e e e))))

(run-tests)