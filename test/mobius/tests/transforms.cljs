(ns mobius.tests.transforms
  (:require [cljs.test :as test :refer-macros [deftest is testing run-tests]]
            [mobius.complex :as c
             :refer [div add minus recip infinity zero one i]]
            [mobius.transforms :as t]))

(deftest identity-transform
  (testing "identity transform"
    (let [I #(t/mult t/I %)
          z (c/complex-rect [1 2])]
      (is (= one (I one)))
      (is (= zero (I zero)))
      (is (= z (I z))))))

(deftest scale-transform
  (testing "scale transform"
    (let [z (c/complex-rect [1 1])
          w (c/complex-rect [-1 1])
          s #(t/mult (t/scale z) %)]
      (is (= w (s i))))))

(comment
  (require '[cljs.test :as t])
  (require '[mobius.tests.transforms])
  (t/run-tests 'mobius.tests.transforms)

  )

(comment
  (require '[mobius.tests.transforms])
  (in-ns 'mobius.tests.transforms)
  (doc is)
  (doc test/run-tests)
  (test/run-tests)
  )

(comment
  (map complex/coords (coords (scale (complex/complex-rect [1 1]))))
  ;;=> ([1 1] [0 0] [0 0] [1 0])

  (complex/coords (det (scale (complex/complex-rect [1 1]))))
  ;;=> [1 1]

  (mult (scale (complex/complex-rect [1 1])) i)
  ;;=> #mobius.complex.complex{:x -1, :y 1}

  (mult (scale (complex/complex-rect [1 1])) (complex/complex-rect [-1 1]))
  ;;=> #mobius.complex.complex{:x -2, :y 0}

  (mult (scale (complex/complex-rect [1 1])) (complex/complex-rect [-2 0]))
  ;;=> #mobius.complex.complex{:x -2, :y -2}
  )

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

(comment
  (mult (translation i) one)
  ;;=> #mobius.complex.complex{:x 1, :y 1}

  (mult (translation i) i)
  ;;=> #mobius.complex.complex{:x 0, :y 2}
  )

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

(comment
  (concentric-circles [0 0] 1 10 1)
  (map #(image-circle T2 (second %)) (concentric-circles [0 0] 1 10 1))

  (map #(image-circle T2 (second %)) (concentric-circles [0 0] .25 2.1 .25))

  (map #(image T2 %) (concentric-circles [0 0] .25 2.1 .25))

  ;; radii from .25 to 4
  (concentric-circles [0 0] .25 2.1 .25)
  (radial-lines 2)
  (radial-lines 4)

  (image T2 unit-circle)
  ;;=> [:line [-6.123233995736766e-17 -1] [-6.123233995736766e-17 1]]
  (image T3 unit-circle)
  ;;=> [:line [1 -6.123233995736766e-17] [-1 -6.123233995736766e-17]]

  )
