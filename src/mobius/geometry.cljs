(ns mobius.geometry
  (:require [mobius.complex :as complex
             :refer [div add minus recip infinity zero one i]]
            [cljs.core.match :refer-macros [match]]))

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

(defn image-circle
  "return the image of given circle C under
  given linear fractional transformation T"
  [T C]
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

(defn collinear?
  "return true if given points are collinear"
  [p1 p2 p3]
  false)

(defn scal-mul
  "multiply a scalar and a vector"
  [t p]
  (let [[x y] p]
    [(* t x) (* t y)]))

(defn plus
  "sum of 2 2-vectors"
  [[x1 x2] [y1 y2]]
  [(+ x1 y1) (+ x2 y2)])

(defn midpoint
  "return midpoint of given two points"
  [p1 p2]
  (scal-mul (/ 2) (plus p1 p2)))

(defn circumcircle
  "return circumcircle of given non collinear points"
  [p1 p2 p3]
  (let [c ()
        r ()]
    [:circle {:center c :radius r}]))

(defn image-line
  "return the image od the given line L
  uder linera fractional transformation T"
  [T L]
  (let [[p1 p2] L
        m (midpoint p1 p2)
        T-fn #(mult T %)
        Tp1 (T-fn p1)
        Tp2 (T-fn p2)
        Tm (T-fn m)]
    (if (collinear? Tp1 Tp2 m)
      [:line (complex/coords Tp1) (complex/coords Tp2)]
      (circumcircle Tp1 Tp2 m))))

(defn image
  "return the image of given generalized circle C
  Under given linear fractional tranformation T"
  [T CorL]
  (match CorL
         [:circle C] (image-circle T C)
         [:line L] (image-line T L)))

(comment
  (let [S1 {:center [0 0] :radius 1}]
    (image-circle T2 S1))
  ;;=> [:line [-6.123233995736766e-17 -1] [-6.123233995736766e-17 1]]

  (let [S1 {:center [0 0] :radius 2}]
    (image-circle T2 S1))
  ;;=> {:center [1.6666666666666665 0], :radius 1.3333333333333333}

  (let [S1 {:center [0 0] :radius .5}]
    (image-circle T2 S1))
  ;;=> {:center [-1.6666666666666665 0], :radius 1.3333333333333333}
  )

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
  (let [l (fn [c1 c2] [:line (complex/coords c1) (complex/coords c2)])]
    (for [i (range n)]
      (let [angle (/ (* i 180) n)
            c1 (complex/complex-polar 4 angle)
            c2 (complex/complex-polar 4 (+ 180 angle))]
        (l c1 c2)))))

(comment
  (concentric-circles [0 0] 1 10 1)
  (map #(image-circle T2 (second %)) (concentric-circles [0 0] 1 10 1))

  (map #(image-circle T2 (second %)) (concentric-circles [0 0] .25 2.1 .25))

  ;; radii from .25 to 4
  (concentric-circles [0 0] .25 2.1 .25)
  (radial-lines 2)
  (radial-lines 4)
  )

;; representation of circles and lines by hermitian matrices
(defn circle-as-matrix
  "return matrix representing circle sith given center and radius"
  [center radius]
  (let [gamma (complex/complex-rect center)
        A 1
        B (minus (complex/conjugate gamma))
        C gamma
        D (- (complex/len-sq center) (* radius radius))]
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
  "return center and radius for given hermitian"
  [[A B C D]]
  (let [c (complex/mult C (/ (- A)))
        c-coords (complex/coords c)
        c-sq (complex/len-sq c-coords)
        r-sq (- c-sq (/ D A))
        r (if (< r-sq 0)
            (complex/sqrt r-sq)
            (Math/sqrt r-sq))
        _ (println "c-sq: " c-sq " r-sq: " r-sq)]
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
  "return line coords [a b c]
  for given line"
  ([l]
   (match l
          [:line p1 p2] (line-coords-from-two-points p1 p2)
          [A B C D] (line-coords-from-matrix l)))
  ([p1 p2] (line-coords-from-two-points p1 p2)))

(defn determinant [[A B C D]]
  (let [b (complex/coords B)
        b2 (complex/len-sq b)]
    (- (* A D) b2)))

(defn point-on-line?
  "check if given point p is on line l
  where l is given by [a b c] with ax + by = c
  being the equation for the line"
  [p l]
  (let [[x y] p
        [a b c] l]
    (= c (+ (* a x) (* b y)))))

(defn mat-inverse
  "matrix inverse of a 2x2 real matrix"
  [[r1 r2]]
  (let [[a b] r1
        [c d] r2
        det (- (* a d) (* b c))
        res [(scal-mul (/ det) [d (- b)])
             (scal-mul (/ det) [(- c) a])]]
    res))

(defn dot [[x1 y1] [x2 y2]]
  (+ (* x1 x2) (* y1 y2)))

(defn mvmult
  "matrix vector multipy"
  [[r1 r2] v]
  [(dot r1 v) (dot r2 v)])

(defn mmmult
  "multiply m, a 2x2 matrix by c
  where c is a vector"
  [m c]
  (let [c00 (get-in c [0 0])
        c10 (get-in c [1 0])
        c01 (get-in c [0 1])
        c11 (get-in c [1 1])
        c1 [c00 c10]
        c2 [c01 c11]
        [r1 r2] m]
    [[(dot r1 c1) (dot r1 c2)]
     [(dot r2 c1) (dot r2 c2)]]))

(defn intersection
  "return the intersection of two lines"
  [l1 l2]
  (let [[a b c] (line-coords l1)
        [d e f] (line-coords l2)
        inv (mat-inverse [[a b] [d e]])
        result (mvmult inv [c f])]
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

  ;; line perpendicular to [2 3]
  (let [b (complex/complex-rect [2 3])
        l (line-as-matrix b)
        l-coords (line-coords l)]
    [(to-string l) (determinant l) l-coords
     (point-on-line? [-3 2] l-coords)
     (point-on-line? [3 -2] l-coords)])

  ;; line through [1 0] [0 1]
  (let [l [:line [1 0] [0 1]]
        l-coords (line-coords l)]
    [l l-coords
     (point-on-line? [1 0] l-coords)
     (point-on-line? [(/ 2) (/ 2)] l-coords)])

  (let [m [[1 2] [3 4]]
        m1 (mat-inverse m)]
    (mmmult m m1))

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
  )
