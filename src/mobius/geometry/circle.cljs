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
  (fn [t] (c/plus (c/mult z (- 1 t)) (c/mult w t))))

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

(defn arc
  "arc between two complex numbers"
  [z1 z2 center radius]
  (let [start (c/arg z1)
        end (+ (c/arg (div z2 z1)))]
    [:arc {:center center :radius radius :start start :end end}]))

(defn render-line
  "render line l consisting of three collinear points
  any of which may be infinity"
  [l color-scheme]
  (let [[z1 z2 z3] l
        lines [(l-style :s1 color-scheme)
               (line z1 z2 z3)

               (l-style :s2 color-scheme)
               (line z2 z3 z1)

               (l-style :s3 color-scheme)
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

(defn render-circle
  "assumes g-circle is not a line"
  [g-circle color-scheme]
  (let [[z1 z2 z3] g-circle
        [p1 p2 p3] (mapv c/coords g-circle)
        circle (circumcircle g-circle)
        {:keys [center radius]} (second circle)
        arcs [(l-style :s1 color-scheme)
              (arc z1 z2 center radius)

              (l-style :s2 color-scheme)
              (arc z2 z3 center radius)

              (l-style :s3 color-scheme)
              (arc z3 z1 center radius)]
        points [(p-style :p1 color-scheme)
                [:point p1]
                (p-style :p2 color-scheme)
                [:point p2]
                (p-style :p3 color-scheme)
                [:point p3]]]
    (concat arcs points)))

(defn render
  "transform generalized circle to
  a sequence of graphics primitives to be rendered"
  ([circle-or-line] (render circle-or-line color-scheme))
  ([circle-or-line color-scheme]
   (if (collinear? circle-or-line)
     (render-line circle-or-line color-scheme)
     (render-circle circle-or-line color-scheme))))

(defn radial-line-from-point [point]
  (let [z (c/complex-rect point)]
    [zero z infinity]))

(defn circle-through-point [point]
  (let [z1 (c/complex-rect point)
        z2 (mult i z1)
        z3 (minus z1)]
    [z1 z2 z3]))

 (comment
  ;; real-axis
  ((render [zero one infinity])

   (render [zero i infinity])
   (render [one i (minus one)])

   (render (radial-line-from-point [-4 -2]))
   ([:style {:stroke "red"}]
    [:line [0 0] [-4 -2]]
    [:style {:stroke "green"}]
    [:line [-4 -2] [-400000000000 -200000000000]]
    [:style {:stroke "blue"}]
    [:line [399999999996 199999999998] [0 0]]
    [:style {:stroke "grey", :fill "cyan"}]
    [:point [0 0]] [:style {:stroke "grey", :fill "magenta"}]
    [:point [-4 -2]])

   (let [T #(t/mult t/J %)
         l1 [zero one infinity]
         l2 [zero i infinity]
         c1 [one i (minus one)]
         tv #(mapv (comp c/coords T) %)]
     (mapv tv [l1 l2 c1]))
   ;;=> [["infinity" [1 0] [0 0]] ["infinity" [0 -1] [0 0]] [[1 0] [0 -1] [-1 0]]]

   (render [zero one infinity])
   ([:style {:stroke "red"}]
    [:line [0 0] [1 0]]
    [:style {:stroke "green"}]
    [:line [1 0] [109 0]]
    [:style {:stroke "blue"}]
    [:line [-108 0] [0 0]]
    [:style {:stroke "grey", :fill "cyan"}]
    [:point [0 0]]
    [:style {:stroke "grey", :fill "magenta"}]
    [:point [1 0]])

   (render [infinity one zero])
   ([:style {:stroke "red"}]
    [:line [109 0] [1 0]]

    [:style {:stroke "green"}]
    [:line [1 0] [0 0]]

    [:style {:stroke "blue"}]
    [:line [0 0] [-108 0]]

    [:style {:stroke "grey", :fill "magenta"}]
    [:point [1 0]]

    [:style {:stroke "grey", :fill "yellow"}]
    [:point [0 0]])

   (let [z (c/complex-rect [1 -1])
         l [zero z infinity]
         T  #(t/mult t/J %)
         Tl (mapv T l)]
     [(mapv c/coords l)
      (mapv c/coords Tl)
      (render l)
      (render Tl)])
   [[[0 0] [1 -1] "infinity"] ["infinity" [0.5 0.5] [0 0]]
    ([:style {:stroke "red"}]
     [:line [500 500] [0.5 0.5]]
     [:style {:stroke "green"}]
     [:line [0.5 0.5] [0 0]]
     [:style {:stroke "blue"}]
     [:line [0 0] [-499.5 -499.5]]
     [:style {:stroke "grey", :fill "magenta"}]
     [:point [0.5 0.5]]
     [:style {:stroke "grey", :fill "yellow"}]
     [:point [0 0]])]

   (render (circle-through-point [1 1]))


   ))
