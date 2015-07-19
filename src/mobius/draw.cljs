(ns mobius.draw
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
            [complex.vector :as v]
            [complex.geometry :as c]))

(enable-console-print!)

;; generic drawing utils
(def clear
  [[:style {:fill "grey"}]
   [:rect [-4 4] [4 -4]]])

(defn clear-screen [draw-chan]
  (go (doseq [d clear]
        (>! draw-chan d))))

(defn render-data
  "render data to drawing channels"
  [app-state draw-chan-1 draw-chan-2 trans done]
  (let [render-list (:render-list app-state)
        ret-chan (chan)
        n (count render-list)]
    (doseq [key render-list]
      (println key))
    (go
      (>! done :done))))

(defn render-mouse-point
  [state draw-chan-1 draw-chan-2 trans]
  (let [rect? (get-in state [:mouse-mode :rectangular])
        polar? (get-in state [:mouse-mode :polar])
        point (:mouse-point state)
        f #(mapv trans %)
        render-list
        (cond-> []
          (and polar? (> (v/len-sq point) 1e-2))
          (into
           (let [line (c/radial-line-through-point point)
                 circle (c/circle-through-point point)]
             [[line c/cs-1] [circle c/cs-2]]))
          rect? (into
                 (let [h-line (c/horizontal-line-through-point point)
                       v-line (c/vertical-line-through-point point)
                       c1 (c/circle-about-point point)]
                   [[h-line c/cs-3] [v-line c/cs-4] [c1 c/cs-5]])))]
    (go
      (doseq [[r cs] render-list]
        (doseq [d (c/render r cs)]
          (>! draw-chan-1 d))
        (doseq [d (c/render (f r) cs)]
          (>! draw-chan-2 d))))))

(defn render-local
  [app-state local-state ch-1 ch-2 trans]
  (let [ret-chan (chan)]
    (go
      (clear-screen ch-1)
      (clear-screen ch-2)
      (render-data app-state ch-1 ch-2 trans ret-chan)
      ;; wait for render-data to complete
      (<! ret-chan)
      ;; now draw local-state on top
      (render-mouse-point local-state ch-1 ch-2 trans))))
