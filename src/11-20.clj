;P11 (*) Modified run-length encoding.
;Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result
; list. Only elements with duplicates are transferred as (N E) lists.
; (encode-modified '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))

(defn encode [coll]  ; copied from P10
  (map #((juxt count first) %) (partition-by identity coll)))

(defn encode-modified [coll]
  (map #(cond (= (first %) 1) (second %) :else (identity %)) (encode coll)))

;(print (encode-modified '(a a a a b c c a a d e e e e)))

;P12 (**) Decode a run-length encoded list.
;Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
; (decode '((4 A) B (2 C) (2 A) D (4 E)))
; (a a a a b c c a a d e e e e)

(defn decode [coll]
  (flatten (map #(cond (seq? %) (repeat (first %) (second %)) :else %) coll)))

;P13 (**) Run-length encoding of a list (direct solution).
;Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists
; containing the duplicates, as in problem P09, but only count them. As in problem P11, simplify the result list by
; replacing the singleton lists (1 X) by X.
; (encode-direct '(a a a a b c c a a d e e e e))
; ((4 A) B (2 C) (2 A) D (4 E))



;P14 (*) Duplicate the elements of a list.
; (dupli '(a b c c d))
; (A A B B C C C C D D)



;P15 (**) Replicate the elements of a list a given number of times.
; (repli '(a b c) 3)
; (A A A B B B C C C)
