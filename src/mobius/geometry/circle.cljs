(ns mobius.geometry.circle
  (:require [mobius.complex :as c
             :refer [mult div add sub minus recip zero one i infinity]]
            [mobius.transforms :as t]
            [mobius.geometry :as g]))

(comment
  (in-ns 'mobius.geometry.circle)
  (let [T #(t/mult t/T3 %)
        l1 [zero one infinity]
        l2 [zero i infinity]
        c1 [one i (minus one)]
        tv #(mapv (comp c/coords T) %)]
    (mapv tv [l1 l2 c1]))
  [[[-1 0] [0 -1] [1 0]]
   [[-1 0] [0 0] [1 0]]
   [[0 -1] [0 0] [0 1]]]
  )

;; generalized circle represented by a vector of three complex numbers

;; trasformed by circle-preserving transformations
;; to another generalized circle
(def color-scheme
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "red"
   :s2 "green"
   :s3 "blue"})

(def cs-1
  {:p1 "cyan"
   :p2 "magenta"
   :p3 "yellow"
   :s1 "blue"
   :s2 "green"
   :s3 "red"})

(def cs-2
  {:p1 "red"
   :p2 "purple"
   :p3 "green"
   :s1 "orange"
   :s2 "purple"
   :s3 "black"})

(defn collinear? [l]
  (let [[z1 z2 z3] l]
    (if (some #(= infinity %) l)
      true
      (g/collinear? z1 z2 z3))))

(defn circumcircle [c]
  (assert (not (collinear? c)))
  (apply g/circumcircle c))

(defn p-style [k color-scheme]
  [:style {:stroke "grey" :fill (k color-scheme)}])

(defn l-style [k color-scheme]
  [:style {:stroke (color-scheme k)}])

(defn param-line
  "the parameterized line between two complex numbers"
  [z w]
  (fn [t] (add (mult z (- 1 t)) (mult w t))))

(defn plus-infinity
  "return largest point on line within user space (r = 4)"
  [z1 z2]
  (let [l (param-line z1 z2)
        l-max (l 100000)
        len (c/length l-max)
        k (/ 6 len)]
    ;;(mult l-max k)
    l-max))

(defn line
  "line between first two complex numbers
  of given line where either one may be infinity"
  [z1 z2 z3]
  (let [[w1 w2]
        (cond
          (= infinity z1) [(plus-infinity z3 z2) z2]
          (= infinity z2) [z1 (plus-infinity z3 z1)]
          :else [z1 z2])]
    [:line (c/coords w1) (c/coords w2)]))

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l color-scheme]
  (let [[z1 z2 z3] l
        lines [(l-style :s1 color-scheme)
               (line z1 z2 z3)

               (l-style :s1 color-scheme)
               (line z2 z3 z1)

               (l-style :s1 color-scheme)
               (line z3 z1 z2)]
        points (cond-> []
                 (not (= infinity z1))
                 (into [(p-style :p1 color-scheme)
                        [:point (c/coords z1)]])

                 (not (= infinity z2))
                 (into [(p-style :p2 color-scheme)
                        [:point (c/coords z2)]])

                 (not (= infinity z3))
                 (into [(p-style :p3 color-scheme)
                        [:point (c/coords z3)]]))]
    (concat lines points)))

(defn arc
  "arc between two complex numbers"
  [center radius start end clockwise]
  [:arc {:center center :radius radius
         :start start :end end
         :clockwise clockwise}])

(defn args
  "arguments of three complex numbers"
  [z1 z2 z3]
  [(c/arg z1) (c/arg z2) (c/arg z3)])

(defn arg-diffs [[z1 z2 z3]]
  [(c/arg (div z2 z1))
   (c/arg (div z3 z2))
   (c/arg (div z1 z3))])

(defn clockwise [c]
  (let [diffs (arg-diffs c)]
    (if (some #(> % (/ c/tau 2)) diffs)
      true false)))

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle color-scheme]
  (let [[z1 z2 z3] g-circle
        [a1 a2 a3] (args z1 z2 z3)
        clockwise? (clockwise g-circle)
        [p1 p2 p3] (mapv c/coords g-circle)
        circle (circumcircle g-circle)
        {:keys [center radius]} (second circle)
        arcs [(l-style :s1 color-scheme)
              (arc center radius a1 a2 clockwise?)

              (l-style :s2 color-scheme)
              (arc center radius a2 a3 clockwise?)

              (l-style :s3 color-scheme)
              (arc center radius a3 a1 clockwise?)]
        circ [(l-style :s1 color-scheme)
              circle]
        points [(p-style :p1 color-scheme)
                [:point p1]
                (p-style :p2 color-scheme)
                [:point p2]
                (p-style :p3 color-scheme)
                [:point p3]]]
    (concat circ points)))

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  ([circle-or-line] (render circle-or-line color-scheme))
  ([circle-or-line color-scheme]
   (if (collinear? circle-or-line)
     (render-line circle-or-line color-scheme)
     (render-circle circle-or-line color-scheme))))

(defn radial-line-through-point [point]
  (let [z (c/complex-rect point)]
    [zero z infinity]))

(defn circle-through-point [point]
  (let [z (c/complex-rect point)
        z1 (mult i z)
        z2 (minus z)
        z3 (mult (minus i) z)]
    [z1 z2 z3]))

(defn horizontal-line-through-point
  [point]
  (let [[x y] point
        z (c/complex-rect point)
        z0 (c/complex-rect [0 y])]
    [z0 z infinity]))

(defn vertical-line-through-point
  [point]
  (let [[x y] point
        z (c/complex-rect point)
        z0 (c/complex-rect [x 0])]
    [z0 z infinity]))

 (comment
   (render [zero one infinity])
   (render [zero i infinity])
   (render [one i (minus one)])
   (render (radial-line-through-point [-4 -2]))

   ;; inversion of real-axis, imaginary-axis, and unit-circle
   (let [T #(t/mult t/J %)
         l1 [zero one infinity]
         l2 [zero i infinity]
         c1 [one i (minus one)]
         tv #(mapv (comp c/coords T) %)]
     (mapv tv [l1 l2 c1]))
   [["infinity" [1 0] [0 0]]
    ["infinity" [0 -1] [0 0]]
    [[1 0] [0 -1] [-1 0]]]

   (mapv c/coords (circle-through-point [1 1]))
   ;;=> [[1 1] [-1 1] [-1 -1]]

   (render (circle-through-point [1 1]))
   (mapv (comp c/rad->deg c/arg) (circle-through-point [1 1]))
   (let [c1 (circle-through-point [1 1])
         T #(t/mult t/J %)
         c2 (mapv T c1)
         f #(mapv c/coords %)
         g #(mapv (comp c/rad->deg c/arg) %)
         d [c1 c2]]
     [(mapv f d)
      (mapv g d)])
   [[[[1 1] [-1 1] [-1 -1]]
     [[0.5 -0.5] [-0.5 -0.5] [-0.5 0.5]]]
    [[45 134.99999999999994 225] [315 225 134.99999999999994]]]

   (mapv c/coords (circle-through-point [1 -1]))
   ;;=> [[1 -1] [1 1] [-1 1]]
   (let [c1 (circle-through-point [1 -1])
         T #(t/mult t/J %)
         c2 (mapv T c1)
         f #(mapv c/coords %)
         g #(mapv (comp c/rad->deg c/arg) %)
         d [c1 c2]]
     [(mapv f d)
      (mapv g d)])
   [[[[1 -1] [1 1] [-1 1]]
     [[0.5 0.5] [0.5 -0.5] [-0.5 -0.5]]]
    [[315 45 134.99999999999994]
     [45 315 225]]]

   (let [c1 (circle-through-point [1 -1])
         T #(t/mult t/J %)
         c2 (mapv T c1)
         f #(mapv c/coords %)
         g #(mapv (comp c/rad->deg c/arg) %)
         h #(arg-diffs %)
         d [c1 c2]]
     [(mapv f d)
      (mapv g d)
      (mapv clockwise d)])
   )
