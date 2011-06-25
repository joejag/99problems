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
  (->>
    (partition 2 1 (cons " " coll))
    (remove #(= (first %) (second %)))
    (map #(second %))
    ))

; P09 (**) Pack consecutive duplicates of list elements into sublists.
; If a list contains repeated elements they should be placed in separate sublists.
; (my-pack '(a a a a b c c a a d e e e e))
; ((A A A A) (B) (C C) (A A) (D) (E E E E))

(defn my-pack
    ([coll] (my-pack coll '()))
    ([coll result]
		(cond
		   (= 0 (count coll)) result
		   (= 0 (count result)) (recur (drop 1 coll) (list (first coll)))
           (= (first coll) (first (last result))) (recur (drop 1 coll) (cons (first coll) result))
           :else (recur (drop 1 coll) (conj (first coll) result))
		)
	)
)

;(print (my-pack '(a a a a b c c a a d e e e e)))

; P10 (*) Run-length encoding of a list.
; Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
;  (encode '(a a a a b c c a a d e e e e))
;  ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))


