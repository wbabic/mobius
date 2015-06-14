(ns mobius.complex
  (:require [mobius.vector :as v]))

;; algebra of complex numbers

(def pi Math/PI)
(def tau (* 2 pi))
(defn mod-tau [x] (mod x tau))

;; Complex arithmetic protocol
(defprotocol Complex
  "protocol for complex number"
  (coords [z] "x y coordinates")
  (plus [z w] "addition of complex numbers")
  (minus [z] "negative a complex number")
  (times [z w] "multiplication of two complex numbers")
  (recip [z] "one over z")
  (conjugate [z] "the conjugate of z")
  (length [z] "the length of z")
  (arg [z] "the argument of z"))

(defn rad->deg [rad]
  (* rad (/ 360 tau)))

(defn deg->rad [deg]
  (* deg (/ tau 360)))

(defn polar->rect
  ([angle] (polar->rect 1 angle))
  ([length angle] [(* length (Math/cos angle))
                   (* length (Math/sin angle))]))

(declare complex-rect)
(declare infinity)
(declare zero)
(declare undefined)

(defrecord complex [x y]
  Complex
  (coords [_] [x y])
  (plus [_ w] (if (= w infinity)
                infinity
                (complex-rect (v/sum [x y] (coords w)))))
  (minus [z] (complex. (- x) (- y)))
  (times [_ w]
    (cond (= w zero) zero
          (= w infinity) infinity
          (number? w) (complex-rect [(* x w) (* y w)])
          :else
          (complex-rect (v/product [x y] (coords w)))))
  (recip [z] (let [d (+ (* x x) (* y y))]
             (complex. (/ x d) (/ (- y) d))))
  (conjugate [_] (complex. x (- y)))
  (length [_] (v/len [x y]))
  (arg [_] (v/angle [x y])))

(defn complex-rect [[x y]]
  (if (and (zero? x) (zero? y))
    zero
    (complex. x y)))

(def one (complex-rect [1 0]))
(def i (complex-rect [0 1]))

(defrecord polar [r alpha]
  Complex
  (coords [_] (polar->rect r (deg->rad alpha)))
  (arg [_] alpha)
  (length [_] r)
  (plus [z w]
    (if (= w infinity)
      infinity
      (complex-rect (v/sum (coords z) (coords w)))))
  (minus [z] (polar. (:length z) (mod (+ alpha 180) 360)))
  (times [_ w]
    (cond (= w zero) zero
          (= w infinity) infinity
          (number? w) (polar. (* r w) alpha)
          :else
          (polar. (* r (length w))
                  (+ alpha (rad->deg (arg w))))))
  (recip [_] (polar. (/ r) (mod (- alpha) 360)))
  (conjugate [_] (polar. r (mod (- alpha) 360))))

(comment
  (coords (plus w1 (minus w1)))
  ;;=> [-1.1102230246251565e-15 6.661338147750939e-16]
  )

(defn complex-polar
  ([arg] (complex-polar 1 arg))
  ([length arg] (polar. length arg)))

(def w1 (complex-polar (/ 360 6)))

(comment
  (instance? complex i)
  (satisfies? Complex i)
  (coords i)
  )

(def zero
  (reify Complex
    (plus [_ w] w)
    (minus [z] z)
    (times [z w]
      (if (= w infinity)
        undefined
        z))  ;; unless w = infinity, then undefined
    (recip [_] infinity)
    (length [_] 0)
    (arg [_] 0)
    (conjugate [z] z)
    (coords [_] [0 0])))

(def infinity
  (reify Complex
    (coords [_] "infinity")
    (plus [z w]
      (if (= w infinity)
        undefined
        z))
    (minus [z] z)
    (conjugate [z] z)
    (times [this w]
      (if (= zero w)
        undefined
        this))
    (recip [_] zero)))

(def undefined
  (reify Complex
    (coords [_] "undefined")))

(comment
  (= zero (recip infinity))
  (identical? zero (recip infinity))
  )

(defn add
  "add complex numbers"
  ([] zero)
  ([z] z)
  ([z w]
   (if (and (= z infinity) (= w infinity))
     undefined
     (plus z w)))
  ([z w & rest] (reduce plus (plus z w) rest)))

(defn mult
  "multiply complex numbers"
  ([] one)
  ([z] z)
  ([z w] (times z w))
  ([z w & rest] (reduce times (times z w) rest)))

(defn div
  "divide two complex numbers"
  [z w]
  (cond
    (and (= z infinity) (= w infinity)) undefined
    (and (= z zero) (= w zero)) undefined
    (= w infinity) zero
    (= z zero) zero
    (= w zero) infinity
    :else (times z (recip w))))

(defn c-dot [[a1 b1] [a2 b2]] (add (mult a1 a2) (mult b1 b2)))

(defn len-sq [z] (mult z (conjugate z)))

(defn distance
  "distance between two complex numbers
  assuming z w not infinity"
  [z w]
  (let [z-w (add z (minus w))
        d (v/len (coords z-w))]
    d))

(defn sqrt
  "square root of given number"
  [n]
  (let [n-pos (Math/abs n)
        n-pos-sqrt (Math/sqrt n-pos)]
    (complex-rect [0 n-pos-sqrt])))

(comment
  (let [z (complex-rect [-1 1])] (mapv coords [z (mult z z)]))
  ;;=> [[-1 1] [0 -2]]

  )

;; inversion in a general circle C
;; where C has center P and radius r
;; is z ->

(defn inversion
  "inversion in a circle"
  ([] (fn [z] (recip (conjugate z))))
  ([circle]
   (let [{:keys [center radius]} circle
         T (inversion)
         Q (complex-rect center)
         r (complex-rect [radius 0])
         S (fn [z] (add (mult r z) Q))
         S-inv (fn [w] (mult (recip r) (add w (minus Q))))]
     (comp S T S-inv))))

(comment
  ;; inversion in unit circle
  (let [T (inversion)]
    (map (comp complex/coords T)
         [zero infinity one i]))

  (let [C {:center [0 1] :radius 1}
        T (inversion C)]
    (map (comp complex/coords T)
         [i infinity one zero (complex/complex-rect [1 1]) (complex/complex-rect [2 1])]))

  )
