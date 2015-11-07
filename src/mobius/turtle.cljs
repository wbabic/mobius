(ns mobius.turtle
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [complex.number :as n]
   [complex.turtle :as turtle]
   [complex.transform :as transform]
   [complex.turtle.render :as render]
   [mobius.render.canvas :as canvas])
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

(def user-space
  {:domain [-2 2]
   :range [-2 2]})

(defn config-for-canvas [xres yres]
  (assoc user-space :resolution [xres yres]))

(defn user->screen-mapping [xres yres]
  (canvas/user->screen (config-for-canvas xres yres)))

(comment
  ((user->screen-mapping 300 300) [0 0])
  ;;=> [150 150]
  )

(defn draw-canvas-contents [ctx w h turtle]
  (let [st @turtle
        render-data (render/render-turtle st)
        t-fn (user->screen-mapping w h)]
    (doseq [d render-data]
      (println d)
      (canvas/render d ctx t-fn))))

(defn draw-turtle [canvas-id turtle]
  (let [canvas (.getElementById js/document canvas-id)
        context (.getContext canvas "2d")
        w (.-clientWidth canvas)
        h (.-clientHeight canvas)]
    (draw-canvas-contents context w h turtle)))

(defn canvas-component [turtle owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (println "canvas-component did update")
      (draw-turtle "canvas2" turtle))
    om/IDidMount
    (did-mount [_]
      (println "mounting canvas-component")
      (draw-turtle "canvas2" turtle))
    om/IWillUnmount
    (will-unmount [_]
      (println "unmounting canvas-component"))
    om/IRender
    (render [_]
      (println "rendering canvas-component")
      (dom/div nil
               (dom/canvas #js {:id "canvas2" :height 300 :width 300})))))

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