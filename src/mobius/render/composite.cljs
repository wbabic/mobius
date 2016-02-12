(ns mobius.render.composite
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
## Compositing and Clipping

fill the inside of a clockwise circle,
where to the left is away from center

same as filling the outside
of a counter-clockwise circle
")

(def user-space
  {:domain [-2 2]
   :range [-2 2]})

(defn config-for-canvas [xres yres]
  (assoc user-space :resolution [xres yres]))

(defn user->screen-mapping [xres yres]
  (canvas/user->screen (config-for-canvas xres yres)))

(def t-fn (user->screen-mapping 300 300))

(comment
  (t-fn [0 0])
  ;;=> [150 150]
  )

(def c1 [n/one n/i n/negative-one])
(def x-axis [n/zero n/one n/infinity])
(def x-axis-style {:edge :blue :inside :lt-blue})
(def y-axis [n/zero n/negative-i n/infinity])
(def y-axis-style {:edge :green :inside :lt-green})
(def c2 [n/one n/negative-i n/negative-one])
(def c1-style {:edge :red :inside :lt-red})

(defn draw-circle [canvas-id circle circle-style]
  (let [canvas (.getElementById js/document canvas-id)
        context (.getContext canvas "2d")]
    (let [render-data (render/render-circle
                       circle
                       circle-style)]
      (doseq [d render-data]
        (canvas/render d context t-fn)))))

(defn draw-line [context line line-style]
  (let [render-data (render/render-line
                     line
                     line-style)]
    (doseq [d render-data]
      (canvas/render d context t-fn))))

(defn draw-inside [context]
  (let [render-edge (render/render-circle-edge [:circle {:center [0 0] :radius 1}] :red)
        render-disk [[:style {:fill :red}] [:disk {:center [0 0] :radius 1}]]
        render-data [[:style {:fill :lt-red}]
                     [:quad [-2 -2] [2 -2] [2 2] [-2 2]]]]
    (doseq [d render-data]
      (canvas/render d context t-fn))
    (.save context)
    (set! (. context -globalCompositeOperation) "destination-out")
    (doseq [d render-disk]
      (canvas/render d context t-fn))
    (.restore context)
    (doseq [d render-edge]
      (canvas/render d context t-fn))))

(defn copy-canvas-to-context [canvas context]
  (.drawImage context canvas 0 0 300 300))

(defn canvas-component [turtle owner]
  (reify
    om/IDidUpdate
    (did-update [this prev-props prev-state]
      (println "canvas-component did update"))
    om/IDidMount
    (did-mount [_]
      (println "mounting canvas-component")
      (let [canvas1 (.getElementById js/document "canvas1")
            context1 (.getContext canvas1 "2d")
            canvas2 (.getElementById js/document "canvas2")
            context2 (.getContext canvas2 "2d")]
        (draw-inside context1)
        (draw-line context2 x-axis x-axis-style)
        ;;(draw-line context2 y-axis y-axis-style)
        (copy-canvas-to-context canvas1 context2)))
    om/IWillUnmount
    (will-unmount [_]
      (println "unmounting canvas-component"))
    om/IRender
    (render [_]
      (println "rendering canvas-component")
      (dom/div nil
               (dom/canvas #js {:id "canvas1" :height 300 :width 300})
               (dom/canvas #js {:id "canvas2" :height 300 :width 300})))))

(defonce st (atom turtle/standard-turtle))

(defcard-om render-light-circle
  "
## Render two light circles

* left) a clockwise circle with inside colored :lt-red

* right) left copied onto a blue x-axis with a :lt-blue inside
"
  canvas-component
  st)
