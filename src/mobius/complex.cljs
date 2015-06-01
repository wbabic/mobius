(ns mobius.complex)

;; linear fractional transformations
;; complex numbers
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

(defn sum
  "sum two vectors"
  [z w] (mapv + z w))

(defn product
  ([] [1 0])
  ([z] z)
  ([z1 z2] (let [[x1 y1] z1
                 [x2 y2] z2]
             [(- (* x1 x2) (* y1 y2))
              (+ (* y1 x2) (* x1 y2))]))
  ([z1 z2 & rest] (reduce product (product z1 z2) rest)))

(defn dot [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))

(defn len-sq [z] (dot z z))

(defn len [z] (Math/sqrt (len-sq z)))

(defn angle [[x y]] (Math/atan2 y x))

(defn rad->deg [rad]
  (* rad (/ 360 tau)))

(defn deg->rad [deg]
  (* deg (/ tau 360)))

(defn polar->rect
  ([angle] (polar->rect 1 angle))
  ([length angle] [(* length (Math/cos angle))
                   (* length (Math/sin angle))]))

(declare complex-rect)

(defrecord complex [x y]
  Complex
  (coords [_] [x y])
  (plus [_ w] (complex-rect (sum [x y] (coords w))))
  (minus [z] (complex. (- x) (- y)))
  (times [z w] (complex-rect (product [x y] (coords w))))
  (recip [z] (let [d (+ (* x x) (* y y))]
             (complex. (/ x d) (/ (- y) d))))
  (conjugate [_] (complex. x (- y)))
  (length [_] (len [x y]))
  (arg [_] (angle [x y])))

(defn complex-rect [[x y]] (complex. x y))

(def one (complex-rect [1 0]))
(def i (complex-rect [0 1]))

(defrecord polar [r alpha]
  Complex
  (coords [_] (polar->rect r (deg->rad alpha)))
  (arg [_] alpha)
  (length [_] r)
  (plus [z w] (complex-rect (sum (coords z) (coords w))))
  (minus [z] (polar. (:length z) (mod (+ alpha 180) 360)))
  (times [_ w] (polar. (* r (length w))
                       (+ alpha (arg w))))
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

(declare infinity)
(declare undefined)

(def zero
  (reify Complex
    (plus [_ w] w)
    (minus [z] z)
    (times [z w] z)  ;; unless w = infinity, then undefined
    (recip [_] infinity)
    (length [_] 0)
    (arg [_] 0)
    (conjugate [z] z)
    (coords [_] [0 0])))

(def infinity
  (reify Complex
    (plus [z w] z);; unless w = infinity, then undefined
    (minus [z] z)
    (times [this w] this)
    (recip [_] zero)))

(comment
  (= zero (recip infinity))
  (identical? zero (recip infinity))
  )

(defn add
  "add complex numbers"
  ([] zero)
  ([z] z)
  ([z w] (plus z w))
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
  (times z (recip w)))
