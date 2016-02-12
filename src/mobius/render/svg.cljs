(ns mobius.render.svg
  (:require [devcards.core]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [complex.number :as n]
            [complex.turtle :as turtle]
            [complex.turtle.render :as render]
            [mobius.render.canvas :as canvas])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc defcard-om]]))

(defcard-doc
  "
## svg

render svg using react

we have points line circles and paths
")

(comment
  (in-ns 'mobius.render.svg)
  )

(defn svg-component
  "an example of a svg component"
  [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/svg #js {:width 300 :height 300}
               (dom/rect #js {:width "100%" :height "100%" :fill "red"})
               (dom/circle #js {:cx 150 :cy 150 :r 50 :fill "blue"}))
      )))

(defcard-om svg-component-card
  "
An svg component, inside a devcard, styled with svg:
"
  svg-component)
