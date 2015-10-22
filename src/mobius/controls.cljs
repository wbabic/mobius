(ns mobius.controls
  (:require
   [om.core :as om :include-macros true]
   [om.dom :as dom :include-macros true]
   [cljs.test :as t :include-macros true :refer-macros [testing is]]
   [mobius.render.canvas :as render]
   [complex.number :as n])
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

(defcard-om user->screen-transform
  "## Transformation from user space to screen space
   ### Enter user space coordinates and calculate screen space image."
  user->screen-component
  initial-state)

(def one n/one)
(def i n/i)
(def negative-one (n/minus one))
(def negative-i (n/minus i))
(def zero n/zero)
(def infinity n/infinity)
(def point-map
  {:one one
   :i i
   :negative-one negative-one
   :negative-i negative-i
   :zero zero
   :infinity infinity})

(def standard-turtle
  {:x-axis [:zero :one :infinity :negative-one]
   :y-axis [:zero :i :infinity :negative-i]
   :unit-circle [:one :i :negative-one :negative-i]})

(defcard standard-turtle
  "## The Standard Turtle"
  standard-turtle)

(comment
  (let [id "canvas1"]
    (dom/div nil
             (dom/canvas #js {:id "canvas1" :height 300 :width 500})))
  )

(defn turtle-transform [data owner]
  (dom/div #js {:className "turtle-transform"}
           (dom/dl nil
                   (dom/h3 nil "Turtle transform sequence")
                   )))

(defcard turtle-state
  "# The State of the Turtle
   ##The state is the transform that maps the standard turtle to this turtle.

    Moving around the turtle,
    creating sequence of transforms that can be reduced to a single transform,
    or replayed back as an animation."
  turtle-transform)

(defcard transformed-turtle
  "## The Transformed Turtle
   ### Draw the transformed turtle to a canvas element."
  (let [id "canvas1"]
    (dom/div nil
             (dom/canvas #js {:id "canvas1" :height 300 :width 500}))))

(defonce observed-atom
  (let [a (atom {:time 0})]
    (js/setInterval (fn [] (swap! observed-atom update-in [:time] inc)) 1000)
    a))

(defcard atom-observing-card observed-atom {} {:history false})
