(ns mobius.transforms
  (:require [cljs.core.match :refer-macros [match]]
            [mobius.complex :as complex
             :refer [div add minus recip infinity zero one i]]
            [mobius.geometry :as g]
            [mobius.vector :as v]))

;; the algebra of
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
  (inverse [T]
    (let [k (det T)
          m #(complex/mult % k)]
      (mobius-trans. (m d)
                     (m (minus b))
                     (m (minus c))
                     (m a))))
  (compose [_ T2] (let [[a2 b2 c2 d2] (coords T2)]
                    (mobius-trans. (complex/c-dot [a b] [a2 c2])
                                   (complex/c-dot [a b] [b2 d2])
                                   (complex/c-dot [c d] [a2 c2])
                                   (complex/c-dot [c d] [b2 d2]))))
  (conjugate [T S] (compose S (compose T (inverse S))))
  (trace [_] (add a d)))

(def I (mobius-trans. one zero zero one))
(defn scale [a] (mobius-trans. a zero zero one))

(def S1 (let [w (complex/complex-rect [1 0.4])]
          (scale w)))

;; J: z -> 1/z
(def J
  (mobius-trans. zero i i zero))
(defn translation [b]
  (mobius-trans. one b zero one))

;; z -> 1/z
(def T1 (mobius-trans. zero one one zero))

;; transformation which maps one and -one to zero and infinity respectively
(def T2 (mobius-trans. one (minus one) one one))

;; Cayley Traansform
(def T3 (mobius-trans. one (minus i) one i))

(defn rotation
  "return the mobius transformation
  of a rotation about origin by
  given degrees"
  ([degrees]
   (scale (complex/complex-polar degrees)))
  ([P degrees]
   (conjugate (rotation degrees) (translation P))))


(defn image-circle
  "return the image of given circle C under
  given linear fractional transformation T"
  [T C]
  (let [{:keys [center radius]} C
        inv-C (complex/inversion C)
        mdc (mult (inverse T) infinity)
        Q (mult T (inv-C mdc))
        Pr (let [[x y] center]
             (complex/complex-rect [(+ x radius) y]))
        Tpr (mult T Pr)]
    (if-not (= infinity Q)
      [:circle {:center (complex/coords Q)
                :radius (complex/distance Q Tpr)}]
      (let [P (complex/complex-rect center)
            r1 (rotation P 90)
            r2 (rotation P -90)]
        [:line
         (complex/coords (mult T (mult r1 mdc)))
         (complex/coords (mult T (mult r2 mdc)))]))))

(defn image-line
  "return the image of the given line L
  under lineer fractional transformation T"
  [T L]
  (let [[p1 p2] L
        _ (print "pre-image line")
        _ (prn L)
        m (v/midpoint p1 p2)
        T-fn #(mult T %)
        c #(complex/complex-rect %)
        Tp1 (T-fn (c p1))
        Tp2 (T-fn (c p2))
        Tm (T-fn (c m))
        _ (print "image points")
        _ (prn (mapv #(complex/coords %) [Tp1 Tm Tp2]))]
    (if (some #(= infinity %) [Tp1 Tp2 Tm])
      (cond (= infinity Tp1) [:line (complex/coords Tp2) (complex/coords Tm)]
            (= infinity Tp2) [:line (complex/coords Tp1) (complex/coords Tm)]
            :else [:line (complex/coords Tp1) (complex/coords Tp2)])
      (if (g/collinear? Tp1 Tp2 Tm)
        [:line (complex/coords Tp1) (complex/coords Tp2)]
        (g/circumcircle Tp1 Tp2 Tm)))))

(defn image
  "return the image of given generalized circle C
  Under given linear fractional tranformation T"
  [T CorL]
  (match CorL
         [:circle C] (image-circle T C)
         [:line p1 p2] (image-line T [p1 p2])
         [:point p] [:point (complex/coords (mult T (complex/complex-rect p)))]))

(def transforms
  [{:name "Identity"
    :text "z -> z"
    :transform I}
   {:name "Inversion"
    :text "z -> 1/z"
    :transform J}
   {:name "Cayley"
    :text "z -> (z-i)/(z+i)"
    :transform T3}
   {:name "T2"
    :text "z -> (z-1)/(z+1)"
    :transform T2}
   {:name "S1"
    :text "z -> (1 + 0.4i)z"
    :transform S1}])
