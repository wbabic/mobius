(ns mobius.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [mobius.tests.transforms]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
        (run-tests
         'mobius.tests.transforms))
    0
    1))
