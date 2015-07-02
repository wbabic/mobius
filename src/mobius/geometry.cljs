(ns mobius.geometry
  (:require [mobius.complex :as complex
             :refer [div add minus recip infinity zero one i]]
            [mobius.vector :as v]
            [cljs.core.match :refer-macros [match]]))

(def unit-circle [:circle {:center [0 0] :radius 1}])
(def real-axis [:line [0 0] [1 0]])
(def imaginary-axis [:line [0 0] [0 1]])
(def points [[:point [0 0]] [:point [1 0]] [:point [0 1]]])
(def lines [real-axis imaginary-axis unit-circle])
(def axis (interleave points lines))

(declare point-on-line?)
(declare line-coords)
(declare intersection)
(defn collinear?
  "return true if given complex numbers are collinear"
  [c1 c2 c3]
  (let [[p1 p2 p3] (mapv complex/coords [c1 c2 c3])
        l (line-coords p1 p2)]
    (point-on-line? p3 l)))

(defn perp-bisector
  "return perp bisector of line segment z w
  where z and w are complex numbers not equal to infinity
  and result is returned as two complex numbers"
  [[z w]]
  (let [m (complex/midpoint z w)
        t #(add m %)
        t-inv #(add (minus m) %)
        r #(complex/mult i %)
        f (comp t r t-inv)]
    [(f z) (f w)]))

(defn circumcircle
  "return circumcircle of given non collinear complex numbers"
  [p1 p2 p3]
  (let [c (intersection (perp-bisector [p1 p2]) (perp-bisector [p2 p3]))
        r (complex/distance p3 (complex/complex-rect c))
        cc [:circle {:center c :radius r}]]
    cc))

(defn concentric-circles
  "generate sequence of circles
  with given center and radii in range"
  [center start end step]
  (let [c (fn [r] [:circle {:center center :radius r}])]
    (for [r (range start end step)]
      (c r))))

(defn radial-lines
  "return n radial lines through orogin"
  [n]
  (let [l (fn [c1] [:line [0 0] (complex/coords c1)])]
    (for [i (range n)]
      (let [angle (/ (* i 180) n)
            c1 (complex/complex-polar angle)
            c2 (complex/complex-polar 4 (+ 180 angle))]
        (l c1)))))

(defn horizontal-lines
  "horizontal lines"
  [step-size]
  (let [h-line (fn [i] [:line [-4 i] [4 i]])]
    (for [i (range -4 4 step-size)]
      (h-line i))))

(defn vertical-lines
  "horizontal lines"
  [step-size]
  (let [v-line (fn [j] [:line [j -4] [j 4]])]
    (for [j (range -4 4 step-size)]
      (v-line j))))

(defn rectangular-point
  [[x y]]
  [[:line [x 0] [x y]]
   [:line [0 y] [x y]]])

(defn polar-point
  [point]
  (let [radius (v/len point)]
    [[:line [0 0] point]
     [:circle {:radius radius :center [0 0]}]]))

;; representation of circles and lines by hermitian matrices
;; from schwerdtfeger
(defn circle-as-matrix
  "return matrix representing circle with given center and radius"
  [center radius]
  (let [gamma (complex/complex-rect center)
        A 1
        B (minus (complex/conjugate gamma))
        C gamma
        D (- (v/len-sq center) (* radius radius))]
    [A B C D]))

(defn line-as-matrix
  "return matrix representing line
  through origin and perpendicular to complex number b"
  [b]
  (let [b-bar (complex/conjugate b)]
    [0 b-bar b 0]))

;; methods that take a generalized circle
(defn to-string [[A B C D]]
  (pr-str [A (complex/coords B) (complex/coords C) D]))

(defn vaild-circle?
  "determine if given vector represents a hermitian matrix
need A and B to be real and B anc C complex conjugates"
  [[A B C D]]
  (assert (and (number? A) (number? D)))
  ;; (= B (complex/conjugate C))
  true)

(defn to-circle
  "return center and radius for given hermitian matrix"
  [[A B C D]]
  (let [c (complex/mult C (/ (- A)))
        c-coords (complex/coords c)
        c-sq (v/len-sq c-coords)
        r-sq (- c-sq (/ D A))
        r (if (< r-sq 0)
            (complex/sqrt r-sq)
            (Math/sqrt r-sq))]
    [:circle {:center c-coords :radius r}]))

(defn line-coords-from-matrix
  "return line cords [a b c]
  for given matrix where ax + by = c"
  [[A B C D]]
  (assert (zero? A))
  (let [[u v] (complex/coords B)
        a (* 2 u)
        b (* (- 2) v)
        c (- D)]
    [a b c]))

(defn line-coords-from-two-points
  "return line coords for line through two given points"
  [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        a (- y2 y1)
        b (- x1 x2)
        c (- (* x1 y2) (* x2 y1))]
    [a b c]))

(defn line-coords
  "return line coords [a b c] for line ax + by = c
  for given line or for line btween two given points,
  as vectors or complex numbers"
  ([l]
   (match l
          [p1 p2]
          (if (vector? p1)
            (line-coords-from-two-points p1 p2)
            (line-coords-from-two-points (complex/coords p1)
                                         (complex/coords p2)))
          [:line p1 p2] (line-coords-from-two-points p1 p2)
          [A B C D] (line-coords-from-matrix l)))
  ([p1 p2]
   (if (vector? p1)
     (line-coords-from-two-points p1 p2)
     (line-coords-from-two-points (complex/coords p1)
                                  (complex/coords p2)))))

(defn param-line
  "given two endpoints return function
  of parameterized linem"
  [A B]
  (fn [t]
    (v/sum A (v/scal-mul t (v/sum B (v/scal-mul -1 A))))))

(defn determinant
  "determinant of a hermitian matrix"
  [[A B C D]]
  (let [b (complex/coords B)
        b2 (v/len-sq b)]
    (- (* A D) b2)))

(defn point-on-line?
  "check if given point p is on line l
  where l is given by [a b c] with ax + by = c
  being the equation for the line"
  [p l]
  (let [[x y] p
        [a b c] l]
    (< (* (- c (+ (* a x) (* b y))) (- c (+ (* a x) (* b y)))) 10e-10)))

(defn intersection
  "return the intersection of two lines"
  [l1 l2]
  (let [[a b c] (line-coords l1)
        [d e f] (line-coords l2)
        inv (v/mat-inverse [[a b] [d e]])
        result (v/mvmult inv [c f])]
    result))

(comment
  ;; unit circle
  (let [S1 (circle-as-matrix [0 0] 1)]
    [(to-string S1) (determinant S1) (to-circle S1)])

  ;; center i radius 1
  (let [c2 [1 i (minus i) 0]]
    [(to-string c2) (determinant c2) (to-circle c2)])

  ;; imaginary circle: center [0 0] radius i
  (let [c3 [1 zero zero 1]]
    [(to-string c3) (determinant c3) (to-circle c3)])
  ;;=> ["[1 [0 0] [0 0] 1]" 1
  ;;=> [:circle {:center [0 0], :radius #mobius.complex.complex{:x 0, :y 1}}]]

  ;; line perpendicular to [2 3]
  (let [b (complex/complex-rect [2 3])
        l (line-as-matrix b)
        l-coords (line-coords l)]
    [(to-string l) (determinant l) l-coords
     (point-on-line? [-3 2] l-coords)
     (point-on-line? [3 -2] l-coords)])
  ;;=> ["[0 [2 -3] [2 3] 0]" -13 [4 6 0] true true]

  ;; line through [1 0] [0 1]
  (let [l [:line [1 0] [0 1]]
        l-coords (line-coords l)]
    [l l-coords
     (point-on-line? [1 0] l-coords)
     (point-on-line? [(/ 2) (/ 2)] l-coords)])
  ;;=> [[:line [1 0] [0 1]] [1 1 1] true true]

  (let [m [[1 2] [3 4]]
        m1 (v/mat-inverse m)]
    (v/mmmult m m1))
  ;;=> [[1 0] [0 1]]

  (let [o [0 0]
        e1 [1 0]
        e2 [0 1]
        l1 [:line o e1]
        l2 [:line o e2]]
    [(intersection l1 l2)])
  ;;=> [0 0]

  (let [o [0 0]
        e1 [1 0]
        e2 [0 1]
        l1 [:line o e1]
        l2 [:line o e1]]
    [(intersection l1 l2)])
  ;;=> [[NaN NaN]]

  (let [l1 [zero one]
        l2 [zero i]]
    [(intersection l1 l2)])
  ;;=> [[0 0]]

  )
