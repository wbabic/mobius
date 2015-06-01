(ns mobius.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :as async :refer [chan <! >! timeout]]
            [mobius.draw :as draw]
            [mobius.geometry :as geom]))

(enable-console-print!)

(println "mobius.core")

(defonce app-state
  (atom
   {:viewing-point [0 0 1] ;; north pole
    :object-plane [0 0 1]  ;; xy plane
    :image-plane [-1 0 0] ;; zy-plane
    }))

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
            draw-chan (om/get-shared owner :draw-chan)]
        (dom/div {}
                 (dom/dl {}
                         (dom/dt {} "input")
                         (dom/dd {} (input :scale scale owner state)))
                 (dom/button #js {:onClick
                                  #(do
                                     (println "button clicked"))}
                             "Button"))))))

(om/root
 mobius-config
 app-state
 {:target (el "mobius-config")
  :shared {:draw-chan (draw/drawing-loop "mobius-canvas-1")}})
