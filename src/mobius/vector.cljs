(ns mobius.vector)
;; vector and matrix operations
;; vector
;; sum product dot len-sq len angle scale-mul

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

(defn scal-mul
  "multiply a scalar and a vector"
  [t p]
  (let [[x y] p]
    [(* t x) (* t y)]))

(defn dot [[x1 y1] [x2 y2]] (+ (* x1 x2) (* y1 y2)))

(defn len-sq [z] (dot z z))

(defn len [z] (Math/sqrt (len-sq z)))

(defn angle [[x y]] (Math/atan2 y x))

(defn midpoint
  "return midpoint of given two points"
  [p1 p2]
  (scal-mul (/ 2) (sum p1 p2)))

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

(defn mat-inverse
  "matrix inverse of a 2x2 real matrix"
  [[r1 r2]]
  (let [[a b] r1
        [c d] r2
        det (- (* a d) (* b c))
        res [(scal-mul (/ det) [d (- b)])
             (scal-mul (/ det) [(- c) a])]]
    res))