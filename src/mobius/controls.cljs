(ns mobius.controls
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs.test :as t :include-macros true :refer-macros [testing is]]
   [mobius.render.canvas :as render]
   [complex.number :as n]
   [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]])
  (:require-macros
   [devcards.core :as dc :refer [defcard defcard-doc defcard-om noframe-doc deftest dom-node]]
   [cljs.core.async.macros :refer [go go-loop alt!]]))

(def config
  {:resolution [300 300]
   :domain [-4 4]
   :range [-4 4]})

(def user->screen-transform-fn
  (render/user->screen config))

(defonce initial-state
  (atom {:t-fn user->screen-transform-fn
         :user-coords [4 -4]
         :screen-coords nil}))

(defn user->screen-component
  [data owner]
  (reify
    om/IInitState
    (init-state [_]
      {:user-coords (:user-coords data)})

    om/IRenderState
    (render-state [this state]
      (let [user-coords (:user-coords state)
             [x y] user-coords
            screen-coords ((:t-fn data) (:user-coords data))]
        (dom/div nil
                 (dom/dl nil
                         (dom/h3 nil "User Space Coordinates")
                         (dom/dt nil "x")
                         (dom/dd nil
                                 (dom/input
                                  #js {:type "text"
                                       :value x
                                       :onChange (fn [e]
                                                   (om/set-state!
                                                    owner
                                                    :user-coords
                                                    [(.. e -target -value)
                                                     (user-coords 1)]))}))
                         (dom/dt nil "y")
                         (dom/dd nil
                                 (dom/input
                                  #js {:type "text"
                                       :value y
                                       :onChange (fn [e]
                                                   (om/set-state!
                                                    owner
                                                    :user-coords
                                                    [(user-coords 0)
                                                     (.. e -target -value)]))}))
                         (dom/button
                          #js {:onClick
                               (fn [_]
                                 (om/update! data :user-coords
                                             (mapv #(js/parseInt %) user-coords)))}
                          "Calculate"))
                 (dom/dl nil
                         (dom/h3 nil "Screen Space Coordinates")
                         (dom/dt nil "x")
                         (dom/dd nil (dom/input #js {:type "text"
                                                     :value (screen-coords 0)}))
                         (dom/dt nil "y")
                         (dom/dd nil (dom/input #js {:type "text"
                                                     :value (screen-coords 1)}))))))))

(comment
  (defcard-om user->screen-transform
    "## Transformation from user space to screen space
   ### Enter user space coordinates and calculate screen space image."
    user->screen-component
    initial-state))

;; turtle-canvas - a place for turtle to be drawn to
(def turtle-chan (chan))

(def shared {:title "Shared title"
             :channel turtle-chan})

(defn canvas-component [data owner]
  (reify
    om/IDidMount
    (did-mount [_]
      (println "mounting"))
    om/IWillUnmount
    (will-unmount [_]
      (println "unmounting this"))
    om/IRender
    (render [_]
      (dom/div nil
               (dom/h3 nil "A canvas!")
               (dom/div nil (str "Shared text: " (om/get-shared owner :title)))
               (dom/canvas #js {:id "canvas2" :height 300 :width 500})
               (dom/button nil "Render")))))

(def x-axis [[:style {:stroke "red"}]
             [:line [0 0] [1 0]]
             [:line [1 0] [100000 0]]
             [:line [-99999 0] [0 0]]])

(defcard render-data
  "Result of running
   ```clojure
   (render (:x-axis standard-turtle))
   ```
is just data:"
  x-axis)

(comment
  (defcard-om turtle-home
    "A canvas in which to draw our turtle. A drawing loop with a core.async channel in which to place drawing primitives which will then be rendered into a canvas element in the Dom. Get the context in IDidMout's did-mount protocol method. Click the render button to send the above data to the drawing loop."
    canvas-component
    standard-turtle
    {:shared shared}))
