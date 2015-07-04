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

(defn product
  ([] [1 0])
  ([z] z)
  ([z1 z2] (let [[x1 y1] z1
                 [x2 y2] z2]
             [(- (* x1 x2) (* y1 y2))
              (+ (* y1 x2) (* x1 y2))]))
  ([z1 z2 & rest] (reduce product (product z1 z2) rest)))

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
          (complex-rect (product [x y] (coords w)))))
  (recip [z] (let [d (+ (* x x) (* y y))]
             (complex. (/ x d) (/ (- y) d))))
  (conjugate [_] (complex. x (- y)))
  (length [_] (v/len [x y]))
  (arg [_] (mod-tau (v/angle [x y]))))

(defn complex-rect [[x y]]
  (if (and (zero? x) (zero? y))
    zero
    (complex. x y)))

(def c complex-rect)

(def one (complex-rect [1 0]))
(def i (complex-rect [0 1]))

(defn complex-polar [degrees]
  (let [rads (deg->rad degrees)]
    (complex-rect (polar->rect rads))))

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

(defn sub [z w]
  (add z (minus w)))

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

(defn c-dot
  "dot product for 2x2 complex vectors"
  [[a1 b1] [a2 b2]]
  (add (mult a1 a2) (mult b1 b2)))

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

(defn midpoint
  "midpoint -f two complex numbers"
  [z w]
  (mult (add z w) (/ 2)))

;; parameterized circles
;; from deaux
(defn param-circle
  "returns a parameterized circle
  given four complex numbers
  where :infinity is an accepted value"
  [a b c d]
  (fn [t]
    (if (= t :infinity)
      (div a c)
      (div (add (mult a t) b)
                   (add (mult c t) d)))))

(defn three-point->param
  [p q r]
  (let [a (mult r (sub p q))
        b (mult q (sub r p))
        c (sub p q)
        d (sub r p)]
    [a b c d]))

(defn circle
  "return a parameterized circle through the trhee given complex numbers"
  [p q r]
  (apply param-circle (three-point->param p q r)))

(defn f [u v]
  (sub (mult u (conjugate v)) (mult v (conjugate u))))

(defn g [a b c d] (sub (mult a (conjugate d)) (mult b (conjugate c))))

(defn param->general
  "return given parameterized circle in general form
  "
  [a b c d]
  (let [ alpha-bar (minus (div (g a b c d) (f c d)))
        beta (minus (div (f a b) (f c d)))]
    [one (conjugate alpha-bar) alpha-bar beta]))

(defn param->standard
  [a b c d]
  (let [[_ alpha alpha-bar beta] (param->general a b c d)]
    [:circle {:center (coords (minus alpha-bar))
              :radius (length (sub (mult alpha alpha-bar) beta))}]))

(comment
  (mapv coords (three-point->param i one (minus one)))
  ;;=> [[1 -1] [-1 -1] [-1 1] [-1 -1]]

  ;; unit circle
  (let [c1 (param-circle i one (minus i) one)
        data [0 1 :infinity -1]]
    (mapv (comp coords c1) data))
  ;;=> [[1 0] [0 1] [-1 0] [0 -1]]

  (let [s1 (circle i one (minus one))
        data [0 1 :infinity -1]]
    (mapv (comp coords s1) data))
  ;;=> [[1 0] [0 1] [-1 0] [0 -1]]

  (mapv coords (apply param->general (three-point->param i one (minus one))))
  ;;=> [[1 0] [0 0] [0 0] [1 0]]

  ;; circle passing through one i and -one is the unit circle
  (apply param->standard (three-point->param i one (minus one)))
  ;;=> [:circle {:center [0 0], :radius 1}]
  )
