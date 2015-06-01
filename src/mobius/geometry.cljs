(ns mobius.geometry
  (:require [mobius.complex :as complex :refer [mult div add minus infinity zero]]))

;; implement the algebra of
;; linear fractional transformations

(defprotocol Mobius
  "protocol for mobius transformations"
  (coords [T] "returns a vector of complex entries of mobius transformation")
  (mult [T z] "apply a mobius transformation to a complex number")
  (compose [T1 T2] "compose two mobius transformations")
  (inverse [T] "return inverse of mobius transformation")
  (conjugate [T S] "return conjugate of T wrt conjugate mapping S"))

(defn c-dot [[a1 b1] [a2 b2]] (add (mult a1 a2) (mult b1 b2)))

(defrecord mobius-trans [a b c d]
  Mobius
  (coords [_] [a b c d])
  (mult [_ z] (cond
                (= z infinity) (div a c)
                (= z (minus (div d c))) infinity
                (= z zero) (div b d)
                :else (/ (add (mult a z) b)
                         (add (mult c z) d))))
  (inverse [_] (mobius-trans. d (minus b) (minus c) a))
  (compose [_ T2] (let [[a2 b2 c2 d2] (coords T2)]
                    (mobius-trans. (c-dot [a a2] [b d2])
                                   (c-dot [a b2] [b d2])
                                   (c-dot [c a2] [d c2])
                                   (c-dot [c b2] [d d2])))))
