(ns mobius.tests.complex
  (:require [cljs.test :as test :refer-macros [deftest is are testing run-tests]]
            [cljs.test.check :as tc]
            [cljs.test.check.generators :as gen]
            [cljs.test.check.properties :as prop :include-macros true]
            [mobius.complex :as c
             :refer [div add minus recip infinity zero one i coords arg length]]))

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
  (test/run-tests))

(comment
  ;; canvas mouse points
  (gen/sample (gen/vector (gen/choose 0 500) 2))
  )
