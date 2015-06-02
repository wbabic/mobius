(ns mobius.geometry
  (:require [mobius.complex :as complex
             :refer [div add minus infinity zero one i]]))

;; implement the algebra of
;; linear fractional transformations

(defprotocol Mobius
  "protocol for mobius transformations"
  (coords [T] "returns a vector of complex entries of mobius transformation")
  (det [_] "ad - bc")
  (mult [T z] "apply a mobius transformation to a complex number")
  (compose [T1 T2] "compose two mobius transformations")
  (inverse [T] "return inverse of mobius transformation")
  (conjugate [T S] "return conjugate of T wrt conjugate mapping S"))

(defn c-dot [[a1 b1] [a2 b2]] (add (complex/mult a1 a2) (complex/mult b1 b2)))

(defrecord mobius-trans [a b c d]
  Mobius
  (coords [_] [a b c d])
  (det [_] (add (complex/mult a d) (minus  (complex/mult b c))))
  (mult [_ z] (cond
                (= z infinity) (div a c)
                (= z zero) (div b d)
                (= z (minus (div b a))) zero
                (= z (minus (div d c))) infinity
                :else (div (add (complex/mult a z) b)
                           (add (complex/mult c z) d))))
  (inverse [_] (mobius-trans. d (minus b) (minus c) a))
  (compose [_ T2] (let [[a2 b2 c2 d2] (coords T2)]
                    (mobius-trans. (c-dot [a b] [a2 c2])
                                   (c-dot [a b] [b2 d2])
                                   (c-dot [c d] [a2 c2])
                                   (c-dot [c d] [b2 d2])))))

(def I (mobius-trans. one zero zero one))

(comment
  ;; identity transformation
  (map complex/coords (coords I))
  ;;=> ([1 0] [0 0] [0 0] [1 0])

  (map complex/coords (coords (compose I I)))
  ;;=> ([1 0] [0 0] [0 0] [1 0])

  (complex/coords (mult I one))
  ;;=> [1 0]

  (complex/coords (mult I zero))
  ;;=> [0 0]

  (complex/coords (mult I i))
  ;;=> [0 1]
  )

(defn scale [a] (mobius-trans. a zero zero one))

(comment
  (def z1 (complex/complex-rect [1 1]))
  (complex/rad->deg (complex/arg z1))
  ;;=> 45

  (def scale-1 (scale z1))
  (map complex/coords (coords scale-1))
  ;;=> ([1 1] [0 0] [0 0] [1 0])

  (complex/coords (mult scale-1 one))
  ;;=> [1 1]

  (complex/coords (mult scale-1 i))
  ;;=> [-1 1]

  (complex/coords (det (scale (complex/complex-rect [1 1]))))
  ;;=> [1 1]

  (mult (scale (complex/complex-rect [1 1])) i)
  ;;=> #mobius.complex.complex{:x -1, :y 1}

  (mult (scale (complex/complex-rect [1 1])) (complex/complex-rect [-1 1]))
  ;;=> #mobius.complex.complex{:x -2, :y 0}

  (mult (scale (complex/complex-rect [1 1])) (complex/complex-rect [-2 0]))
  ;;=> #mobius.complex.complex{:x -2, :y -2}

  )


(def inversion
  (mobius-trans. zero i i zero))

(comment
  (complex/coords (mult inversion zero))
  ;;=> "infinity"

  (complex/coords (mult inversion infinity))
  ;;=> [0 0]

  (complex/coords (mult inversion one))
  ;;=> [1 0]

  (complex/coords (mult inversion i))
  ;;=> [0 -1]

  )

(defn translation [b]
  (mobius-trans. one b zero one))

(comment
  (mult (translation i) one)
  ;;=> #mobius.complex.complex{:x 1, :y 1}

  (mult (translation i) i)
  ;;=> #mobius.complex.complex{:x 0, :y 2}
  )


;; transformation which maps one and -one to zero and infinity respectively
(def T2 (mobius-trans. one (minus one) one one))

(comment
  (complex/coords (mult T2 one))
  ;;=> [0 0]

  (complex/coords (mult T2 (minus one)))
  ;;=> "infinity"

  ;; has fixed points at i and -i
  (complex/coords (mult T2 i))
  ;;=> [0 1]
  (complex/coords (mult T2 (minus i)))
  ;;=> [0 -1]
  )
