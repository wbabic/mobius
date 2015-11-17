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

A turtle consists of six points, each point being represented by a complex number

and three 'genrealized circles' passing through four of the above points

each circle intersecting the others twice
"
  turtle/standard-turtle)

(defcard render-turtle-as-data
  "
## Render Turtle

as a sequence of graphics data primitives using:

```clojure
  (complex.turtle.render/render-turtle complex.turtle/standard-turtle)
```

output is a sequence of vectors with keywords :point :line :circle, or :style
and associated values

that can be rendered to a canvas or to svg

here is the data for the x-axis:
"
  (render/render-line [n/zero n/one n/infinity] {:edge :blue :inside :lt-blue}))

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

(defcard-om render-turtle-to-canvas
  "
## Render Turtle

### into a canvas

using oriented circles with lightly colored insides,

where inside is to the left,

and a line is just a circle that happens to pass through infinity.

(infinity not shown)
"
  canvas-component
  st)

(defcard transform-turtle
  "
## Transform Turtle

### what happens when a good turtle meets a Mobius transformation

transform each of the point values in the point-map
by applying a Mobius transformation to each complex number,
keeping the keywords the same

the circles have the same keywords but now they refer to the tranformed point values

select a transformation and draw the transformed turtle into a canvas")

(comment
  ;; from figwheel repl
  (in-ns 'mobius.turtle)
  )
