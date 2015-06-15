(ns mobius.tests.transforms
  (:require [cljs.test :as test :refer-macros [deftest is testing run-tests]]
            [mobius.transforms :as t]))

(deftest a-test
  (testing "transforms"
    (is (= 1 1.0))))
