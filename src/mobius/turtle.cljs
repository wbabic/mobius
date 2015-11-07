(ns mobius.turtle
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [complex.number :as n]
   [complex.turtle :as turtle]
   [complex.transform :as transform]
   [complex.turtle.render :as render])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc defcard-om]]))

(defcard turtle-definition
  "
## Mobius Turtle

### what happens when a good turtle meets a Mobius transformation

turtle has keys of :circle :points :style"
  turtle/standard-turtle)

(defcard turtle-rendered-data
  "
## Render Turtle

### as data

a sequence of graphics data primitives:

```clojure
  (render/render-turtle turtle/standard-turtle)
```
")

(defn draw-canvas-contents [ canvas ]
  (let [ ctx (.getContext canvas "2d")
        w (.-clientWidth canvas)
        h (.-clientHeight canvas)]
    (.beginPath ctx)
    (.moveTo ctx 0 0)
    (.lineTo ctx w h)
    (.moveTo ctx w 0)
    (.lineTo ctx 0 h)
    (.stroke ctx)))

(defn get-context-from-id [canvas-id]
  (let [canvas (.getElementById js/document canvas-id)
        context (.getContext canvas "2d")]
    (draw-canvas-contents canvas)))

(defn canvas-component [data owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (println "canvas-component did update")
      (get-context-from-id "canvas2"))
    om/IDidMount
    (did-mount [_]
      (println "mounting canvas-component")
      (get-context-from-id "canvas2"))
    om/IWillUnmount
    (will-unmount [_]
      (println "unmounting canvas-component"))
    om/IRender
    (render [_]
      (println "rendering canvas-component")
      (dom/div nil
               (dom/canvas #js {:id "canvas2" :height 300 :width 500})))))

(defonce st (atom turtle/standard-turtle))

(defcard-om turtle-rendered-to-canvas
  "
## Render Turtle

draw standard turtle into a canvas
"
  canvas-component
  st)

(defcard transform-turtle
  "
## Transform Turtle

geometricly transform point-map
by applying a Mobius transformation to each of its point values
which are represented by complex numbers")

(comment
  (in-ns 'mobius.turtle)
  )
