(ns mobius.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [chan <! >! timeout]]
            [mobius.draw :as draw]
            [mobius.geometry :as geom]))

(enable-console-print!)

(println "mobius.core")

(def canvas-1-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(def canvas-2-config
  {:resolution [500 500]
   :domain [-4 4]
   :range [-4 4]})

(defonce app-state
  (atom
   {:mobius geom/T2}))

(defn el [id] (js/document.getElementById id))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  (println "reloading mobius.core ...")
  )

(defn update-local-state [e owner key state]
  (om/set-state! owner key (.. e -target -value)))

(defn input [key value owner state]
  (dom/input #js {:type "text"
                  :value value
                  :onChange
                  #(update-local-state % owner key state)}))

(def clear
  [[:style {:fill "grey"}]
   [:rect [-4 4] [4 -4]]])

(defn clear-screen [draw-chan]
  (go (doseq [d clear]
        (>! draw-chan d))))

(defn concentric-circles
  "generate sequence of circles
  with given center and radii in range"
  [center radii]
  (let [c (fn [r] [:circle {:center center :radius r}])]
    (for [r radii]
      (c r))))

(def colors ["red" "orange" "yellow" "green" "blue" "indigo" "violet"])

(defn draw-concentric-circles
  "send a sequence of circles to the drawing channel"
  [draw-chan-1 draw-chan-2]
  (clear-screen draw-chan-1)
  (clear-screen draw-chan-2)
  (let [circles (concentric-circles [0 0] (sort [1 1.5 (/ 2 3) 2 0.50 4 0.25]))
        trans #(geom/image geom/T2 (second %))]
    (go
      (doseq [[c color] (map vector circles colors)]
        (<! (timeout 800))
        (>! draw-chan-1 [:style {:stroke color :lineWidth 2}])
        (>! draw-chan-1 c)
        (>! draw-chan-2 [:style {:stroke color :lineWidth 2}])
        (>! draw-chan-2 (trans c))))))

(defn mobius-config
  "input form for mobius"
  [app-state owner]
  (reify
    om/IInitState
    (init-state [_]
      (let [scale (:scale @app-state)]
        {:scale scale}))
    om/IRenderState
    (render-state [_ state]
      (let [scale (:scale state)
            draw-chan-1 (om/get-shared owner :draw-chan-1)
            draw-chan-2 (om/get-shared owner :draw-chan-2)]
        (dom/div {}
                 (dom/dl {}
                         (dom/dt {} "input")
                         (dom/dd {} (input :scale scale owner state)))
                 (dom/button #js {:onClick
                                  #(do
                                     (println "drawing circles")
                                     (draw-concentric-circles draw-chan-1 draw-chan-2))}
                             "Button"))))))

(om/root
 mobius-config
 app-state
 {:target (el "mobius-config")
  :shared {:draw-chan-1 (draw/drawing-loop "mobius-canvas-1" canvas-1-config)
           :draw-chan-2 (draw/drawing-loop "mobius-canvas-2" canvas-2-config)}})

(comment
  (in-ns 'mobius.core)
  ;; test out user-screen mapping
  (let [m (draw/user->screen canvas-1-config)] [(m [0 0]) (m [1 0]) (m [0 1]) (m 1)])
  ;;=> [[250 250] [275 250] [250 225] 25]

  (let [m (draw/user->screen canvas-1-config)
        tf (map m)
        data [[0 0] [1 0] [0 1] 1 [-10 10] [10 -10]]]
    (sequence tf data))
  ;;=> ([250 250] [275 250] [250 225] 25 [0 0] [500 500])

  ;; test out t-fn
  (let [t-fn (draw/transform-fn canvas-1-config)
        data [[0 0] [1 0] [0 1] 1]]
    (sequence t-fn data))
  ;;=> ([250 250] [275 250] [250 225] 25)

  )
