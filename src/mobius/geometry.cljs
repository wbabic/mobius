(ns mobius.geometry
  (:require [mobius.complex :as complex
             :refer [div add minus recip infinity zero one i]]))

;; implement the algebra of
;; linear fractional transformations

(defprotocol Mobius
  "protocol for mobius transformations"
  (coords [T] "returns a vector of complex entries of mobius transformation")
  (det [_] "ad - bc")
  (mult [T z] "apply a mobius transformation to a complex number")
  (compose [T1 T2] "compose two mobius transformations")
  (inverse [T] "return inverse of mobius transformation")
  (conjugate [T S] "return conjugate of T wrt conjugate mapping S")
  (trace [T] "trace of a mobius transform"))

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
                                   (c-dot [c d] [b2 d2]))))
  (conjugate [T S] (compose S (compose T (inverse S))))
  (trace [_] (add a d)))

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

;; J: z -> 1/z
(def J
  (mobius-trans. zero i i zero))

(comment
  (complex/coords (mult J zero))
  ;;=> "infinity"

  (complex/coords (mult J infinity))
  ;;=> [0 0]

  (complex/coords (mult J one))
  ;;=> [1 0]

  (complex/coords (mult J i))
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

  ;; zero -> -one
  (complex/coords (mult T2 zero))
  ;;=> [-1 0]

  ;; infinity -> one
  (complex/coords (mult T2 infinity))
  ;;=> [1 0]

  ;; has fixed points at i and -i
  (complex/coords (mult T2 i))
  ;;=> [0 1]
  (complex/coords (mult T2 (minus i)))
  ;;=> [0 -1]

  (complex/coords (trace T2))
  ;;=> [2 0]
  )

;; inversion in a general circle C
;; where C has center P and radius r
;; is z ->

(defn inversion
  "inversion in a circle"
  ([] (fn [z] (complex/recip (complex/conjugate z))))
  ([circle]
   (let [{:keys [center radius]} circle
         T (inversion)
         Q (complex/complex-rect center)
         r (complex/complex-rect [radius 0])
         S (fn [z] (add (complex/mult r z) Q))
         S-inv (fn [w] (complex/mult (recip r) (add w (minus Q))))]
     (comp S T S-inv))))

(comment
  (comp S T S-inv)
  (fn [z]
    (let [z1 (S-inv z)
          z2 (T z1)
          z3 (S z2)]
      z3))
  ;; inversion in unit circle
  (let [T (inversion)]
    (map (comp complex/coords T)
         [zero infinity one i]))

  (let [C {:center [0 1] :radius 1}
        T (inversion C)]
    (map (comp complex/coords T)
         [i infinity one zero (complex/complex-rect [1 1]) (complex/complex-rect [2 1])]))

  )

(defn rotation
  "return the mobius transformation
  of a rotation about origin by
  given degrees"
  ([degrees]
   (scale (complex/complex-polar degrees)))
  ([P degrees]
   (conjugate (rotation degrees) (translation P))))

(comment
  (let [r-trans (scale i)
        r #(mult r-trans %)]
    (map (comp complex/coords r) [zero one i (minus one) (minus i)]))
  ;;=> ([0 0] [0 1] [-1 0] [0 -1] [1 0])

  (let [r-trans (rotation 90)
        r #(mult r-trans %)]
    (map (comp complex/coords r) [zero one i (minus one) (minus i)]))
  ;;=> ([0 0] [6.123233995736766e-17 1] [-1 1.2246467991473532e-16] [6.123233995736766e-17 -1] [1 0])

  (complex/mult (complex/complex-polar 90) i)
  ;;=> #mobius.complex.polar{:r 1, :alpha 180}

  ;; rotate about i by 90 degrees
  (let [r1-trans (rotation i 90)
        r1 #(mult r1-trans %)]
    (map (comp complex/coords r1) [i zero one]))
  ;;=> ([0 1] [1 1] [1 2])
  )

;; image of a circle under T
(defn image [T C]
  (let [{:keys [center radius]} C
        inv-C (inversion C)
        mdc (mult (inverse T) infinity)
        Q (mult T (inv-C mdc))
        Pr (let [[x y] center]
             (complex/complex-rect [(+ x radius) y]))
        Tpr (mult T Pr)
        new-radius (complex/distance Q Tpr)]
    (if-not (= infinity Q)
      [:circle {:center (complex/coords Q)
                :radius new-radius}]
      (let [P (complex/complex-rect center)
            r1 (rotation P 90)
            r2 (rotation P -90)]
        [:line (complex/coords (mult r1 mdc)) (complex/coords (mult r2 mdc))]))))

(comment
  (let [S1 {:center [0 0] :radius 1}]
    (image T2 S1))
  ;;=> {:center "infinity"}

  (let [S1 {:center [0 0] :radius 2}]
    (image T2 S1))
  ;;=> {:center [1.6666666666666665 0], :radius 1.3333333333333333}

  (let [S1 {:center [0 0] :radius .5}]
    (image T2 S1))
  ;;=> {:center [-1.6666666666666665 0], :radius 1.3333333333333333}

  )
