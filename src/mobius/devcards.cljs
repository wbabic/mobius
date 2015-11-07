(ns mobius.devcards
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs.test :as t :include-macros true :refer-macros [testing is]]
   [mobius.turtle])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc defcard-om noframe-doc deftest dom-node]]))

(defcard-doc
  "## This is a markup devcard

    And this is a sentence.")
