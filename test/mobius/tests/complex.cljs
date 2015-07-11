(ns mobius.tests.complex
  (:require [cljs.test :as test :refer-macros [deftest is are testing run-tests]]
            [cljs.test.check :as tc]
            [cljs.test.check.properties :as prop :include-macros true]
            [cljs.test.check.generators :as gen]
            [cljs.test.check.rose-tree :as rose]
            [cljs.test.check.cljs-test :refer-macros [defspec]]
            [mobius.complex :as c
             :refer [mult div add sub minus recip infinity zero one i coords arg length]]))

(deftest test-arg
  (testing "complex arg"
    (let [z1 (c/c [1 1])
          z2 (c/c [1 -1])]
      (are [a b] (= a b)
           45 (c/rad->deg (arg z1))
           315 (c/rad->deg (arg z2))
           180 (c/rad->deg (arg (minus one)))
           90(c/rad->deg (arg i))))))

(deftest div-by-zero
  (testing "divide by zero"
    (is (= infinity (div 1 zero)))
    (is (= zero (div 1 infinity)))))

(comment
  (require '[mobius.tests.complex])
  (in-ns 'mobius.tests.complex)
  (test/run-tests)
  )

(comment
  ;; canvas mouse points
  (gen/sample (gen/vector (gen/choose 0 500) 2))

  ;; random points in user space
  (gen/sample (gen/vector gen/ratio 2))

  (let [f-prop (prop/for-all [numbers (gen/list gen/nat)]
                             (= (first numbers) (peek numbers)))]
    (tc/quick-check 100 f-prop))
  )

(def complex-gen
  "generate a complex number"
  (gen/fmap c/complex-rect (gen/vector gen/ratio 2)))

(def non-zero-complex-gen
  (gen/such-that #(not (= zero %)) complex-gen))

(comment
  (gen/sample complex-gen)
  (map c/coords (gen/sample complex-gen))
  (map c/coords (gen/sample non-zero-complex-gen))
  (map #(instance? c/complex %) (gen/sample complex-gen))
  )

(def vector-and-elem (gen/bind (gen/not-empty (gen/vector gen/int))
                               #(gen/tuple (gen/return %) (gen/elements %))))

(comment
  (gen/sample vector-and-elem)
  )

(def gen-circle
  (gen/vector complex-gen 3))

(comment
  (gen/sample gen-circle)
  )

(def complex-prop-1
  (prop/for-all [z complex-gen]
                (instance? c/complex z)))

(def complex-prop-2
  (prop/for-all [z complex-gen]
                (satisfies? c/Complex z)))

;; test our generators
(comment
  (tc/quick-check 100 complex-prop-1)
  ;;=> false
  (tc/quick-check 100 complex-prop-2)
  ;;=> true

  )

(defspec complex-gen-satisfies-protocol complex-prop-2)

;; test inverses, additive and multiplicative
(def additive-inverse-prop
  (prop/for-all [z complex-gen]
                (= zero (add z (minus z)))))

(defspec additive-inverse additive-inverse-prop)

(defn almost-zero? [z]
  (let [[x y] (coords z)
        epsilon 10e-15
        f (fn [x] (< (Math/abs x) epsilon))]
    (and (f x) (f y))))

(def mult-inverse-prop
  (prop/for-all [z non-zero-complex-gen]
                (let [p (mult z (recip z))
                      r (sub p one)]
                  (almost-zero? r))))

(defspec mult-inverse mult-inverse-prop)

(comment
  (let [z (c/c [.5 -1.5]) rz (recip z)] (mapv coords [z rz (mult z rz)]))
       )
